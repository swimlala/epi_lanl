CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2021-10-02T09:00:51Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p4   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �T   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �,   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ݼ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �x   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �|   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20211002090051  20211002090051  5906101 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               VA   AO  7907                            2B  A   NAVIS_A                         1015                            170425                          863 @ٗ���1   @ٗզ��@&� ě���dO�;dZ1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         VA   A   A   @�  @�  A   A   A@  A`  A�  A�  A���A�  A�  A�  A���A�33A�33B  B  B  B   B(  B0  B8  B@  BH��BM��BW��B`  Bh  Bp  Bx  B�33B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B���B�  B�  B���B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C]�fC`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D_��D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ Dܼ�D�  D�C3D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�p�@�p�@�p�A�RA:�RAZ�RAz�RA�\)A�(�A�\)A�\)A�\)A�(�A�\A��\B�B�B�B�B&�B.�B6�B>�BGz�BLG�BVG�B^�Bf�Bn�Bv�BzB��=B�#�B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
Bϊ=Bӊ=B�#�B�W
B�W
B�#�B�W
B�W
B�W
B�W
B�W
B�W
B�W
C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D j�D ��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��D	j�D	��D
j�D
��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��D j�D ��D!j�D!��D"j�D"��D#j�D#��D$j�D$��D%j�D%��D&j�D&��D'j�D'��D(j�D(��D)j�D)��D*j�D*��D+j�D+��D,j�D,��D-j�D-��D.j�D.��D/j�D/��D0j�D0��D1j�D1��D2j�D2��D3j�D3��D4j�D4��D5j�D5��D6j�D6��D7j�D7��D8j�D8��D9j�D9��D:j�D:��D;j�D;��D<j�D<��D=j�D=��D>j�D>��D?j�D?��D@j�D@��DAj�DA��DBj�DB��DCj�DC��DDj�DD��DEj�DE��DFj�DF��DGj�DG��DHj�DH��DIj�DI��DJj�DJ��DKj�DK��DLj�DL��DMj�DM��DNj�DN��DOj�DO��DPj�DP��DQj�DQ��DRj�DR��DSj�DS��DTj�DT��DUj�DU��DVj�DV��DWj�DW��DXj�DX��DYj�DY��DZj�DZ��D[j�D[��D\j�D\��D]j�D]��D^j�D^��D_j�D_�{D`j�D`��Daj�Da��Dbj�Db��Dcj�Dc��Ddj�Dd��Dej�De��Dfj�Df��Dgj�Dg��Dhj�Dh��Dij�Di��Djj�Dj��Dkj�Dk��Dlj�Dl��Dmj�Dm��Dnj�Dn��Doj�Do��Dpj�Dp��Dqj�Dq��Drj�Dr��Dsj�Ds��Dtj�Dt��Duj�Du��Dvj�Dv��Dwj�Dw��Dxj�Dx��Dyj�Dy��Dzj�Dz��D{j�D{��D|j�D|��D}j�D}��D~j�D~��Dj�D��D�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��>D��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD���D���D�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqDµqD��qD�5qD�uqDõqD��qD�5qD�uqDĵqD��qD�5qD�uqDŵqD��qD�5qD�uqDƵqD��qD�5qD�uqDǵqD��qD�5qD�uqDȵqD��qD�5qD�uqDɵqD��qD�5qD�uqDʵqD��qD�5qD�uqD˵qD��qD�5qD�uqD̵qD��qD�5qD�uqD͵qD��qD�5qD�uqDεqD��qD�5qD�uqDϵqD��qD�5qD�uqDеqD��qD�5qD�uqDѵqD��qD�5qD�uqDҵqD��qD�5qD�uqDӵqD��qD�5qD�uqDԵqD��qD�5qD�uqDյqD��qD�5qD�uqDֵqD��qD�5qD�uqD׵qD��qD�5qD�uqDصqD��qD�5qD�uqDٵqD��qD�5qD�uqDڵqD��qD�5qD�uqD۵qD��qD�5qD�uqDܲ>D��qD�8�D�uqDݵqD��qD�5qD�uqD޵qD��qD�5qD�uqDߵqD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD���D�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD��qD��qD�8�D�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�8�D�o11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A❲A��A��A��A��A��A��A��A��A��A��A��A◍A�I�A��A۬A֛�A�%A�M�A�%A�z�A�"�A�K�A��A�x�A���A�l�A��A���A�K�A��;A�A�%A��PA��A�Q�A�(�A���A��A�+A��7A��
A�ZA��FA���A���A��A��7A�r�A�=qA��yA���A�$�Av�DAq�An=qAh�Ae�PAa��A^�`A]��A\JAY�AW�TAU?}AQ��AOp�AN�+AM�AKS�AH�HAF�+AD��AB^5A;K�A:��A=/A>$�A=�^A="�A<��A<-A;dZA:-A8A�A6�/A6�+A6VA5�wA3\)A2�A2^5A1�A1`BA0��A0M�A0 �A/��A.��A-��A-|�A-hsA-G�A-��A-"�A,��A,�RA,��A,�+A,^5A,�A+�A+�
A+��A+�7A+XA*�jA)��A)|�A)%A(��A(z�A'�A'�A'oA&�HA&VA%��A%ƨA%��A%K�A%&�A$��A$�9A$bA#��A#��A#S�A#�A"�A"��A"��A"��A"��A"jA"E�A"=qA!��A!G�A!/A!�A!VA ��A ȴA v�A �A�A�;A�FA��A�uAI�A$�A��A\)A�AA�jAA�A�;AƨAA��A�hAl�AG�A�A%A�`A�A�\AQ�A�mAXA��A��A�jA�!AjAAhsA33A�A�DAI�A�A\)A+A�AVA��AI�AJA�
A�hA7LA��A�/A��A �A1A�
A�7A`BA"�A�+A�A�7AVA�!A^5A�AM�At�A
�A
�9A
�A	��A	�7A	K�A��A�HA��AZA �A�hA33A��AjA��Ax�AO�A�A�!A�A�!AA�A$�A �A  A�A33A
=A �uA �@���@�t�@���@�$�@��h@�j@���@�|�@�K�@��R@�$�@��7@�7L@���@�1@���@���@��@��@�`B@�G�@��@���@�+@���@�V@�?}@��D@�Z@�1@@���@�n�@�{@�G�@�@�ƨ@�~�@�@�@�7L@�z�@��;@��@�V@��@�1@���@�F@�
=@�@�@�G�@�Ĝ@�(�@���@ߝ�@��H@�~�@�ff@�^5@�E�@ݡ�@ܴ9@���@��@�@���@ٲ-@���@�\)@թ�@�V@�z�@�\)@ҸR@Ѳ-@ѩ�@�&�@�Ĝ@� �@ϝ�@�S�@��@��@�=q@��@͙�@�O�@��/@�j@���@ˍP@�\)@�+@���@���@ʗ�@��@��`@ȣ�@��m@Ǿw@ǝ�@���@��y@�$�@�x�@���@�j@��m@��@�$�@���@���@�r�@���@�+@�M�@��@��@��`@�1@�ƨ@�C�@�ȴ@��+@�-@��@�hs@�/@��/@��D@�Q�@��@���@��@���@���@�
=@��H@�ff@�`B@���@��j@��@���@�j@��
@�o@���@��R@�v�@�ff@�E�@�-@��@�O�@���@�1@�K�@�+@�o@�~�@��#@�`B@��@���@�z�@�9X@�(�@��@���@�33@���@���@�n�@�5?@��#@��h@�%@���@�(�@���@�n�@��@���@�O�@�V@��@�bN@�9X@��
@�@�n�@��@�@�hs@��`@���@�I�@��;@���@�dZ@�o@���@�~�@�ff@�=q@�@�@��@�`B@���@�j@�1@��@�o@��!@��\@�5?@���@��7@�V@��`@�Ĝ@��j@��@���@��;@�S�@��@�-@��#@���@��@�/@�V@���@�9X@�1@��m@��F@�\)@��H@���@���@��R@��+@��-@�`B@�`B@��@��9@��@�j@�I�@��m@��@�v�@�5?@���@���@�G�@��@���@��9@��@�j@�bN@�1'@�K�@���@�=q@�J@��@���@�`B@�&�@��@�%@��@� �@��;@��F@�|�@�;d@�@���@��@���@���@�~�@�ff@�J@�O�@�7L@�V@���@���@��@��u@��@�z�@�r�@�bN@�1'@�(�@�1@��w@��P@�dZ@�K�@�33@�
=@���@�ff@�M�@�=q@�-@��@�@�`B@�X@�O�@�/@���@��`@��@�(�@�  @�w@�P@\)@
=@~ȴ@~ff@~{@}�h@}�@|�@|z�@|9X@{��@{�@z�!@y�#@y7L@x��@x��@x��@x�9@xb@v��@vv�@v5?@u��@u�h@s�
@sC�@s@r�@r��@r��@r�!@r^5@q�#@q&�@p1'@o�w@o
=@n�+@n$�@m@m`B@k�m@k��@kS�@j�!@i��@i%@hĜ@hbN@g�P@fff@e�T@e�@d�j@cƨ@co@b��@bn�@b^5@bM�@b-@b�@bJ@a��@aX@`��@_�;@_�@^��@^��@^{@]p�@\�j@[�m@[o@Z�!@Z�@Y�@Y��@YX@X��@XQ�@W�;@W�;@W�w@W|�@W�@V��@U��@T�/@T��@T��@T��@T(�@S�
@S��@SS�@SC�@S"�@S@R�H@R��@R�!@R��@R��@R~�@RM�@R�@Q��@Q%@Pb@O�w@O
=@N�y@N��@Nff@N@M��@Mp�@M�@L�@L��@LI�@L(�@K�m@K�@K33@JM�@I�@I�7@I7L@H��@H�u@H1'@H1'@G�;@G�P@G\)@G;d@Fȴ@FE�@F@E��@EO�@E/@D�/@D��@Dz�@D9X@D1@C��@CC�@C"�@B��@B�!@Bn�@B-@A��@A�@A�^@A�7@A��@A�^@@��@?�@?�@?�P@?|�@?\)@?;d@?�@>�@>ȴ@>�R@>��@>�+@>5?@=�-@=O�@<��@<�@<j@;�m@;S�@;@:��@:��@:��@:n�@9�#@9&�@8�`@8�@8bN@81'@8b@7��@7�@6�@6v�@6$�@5�T@5�-@5�h@5?}@4�@4��@49X@4�@3�@3S�@3C�@3C�@3@2��@2��@2~�@2J@1�7@0Ĝ@0�@0r�@01'@0 �@/�@/�w@/�@/�@/�@/��@/�P@/l�@/l�@/l�@/\)@/;d@.ȴ@.��@.v�@.V@.5?@.$�@-p�@-V@,�/@,�@,z�@,(�@+ƨ@+o@*��@*n�@*M�@*�@)��@)��@)��@)��@)x�@)hs@)7L@(��@(Ĝ@(��@(Q�@(A�@(1'@'�@'�w@'\)@';d@';d@';d@'+@'�@&��@&��@&��@&�y@&�y@&�@&��@&v�@&E�@&$�@&{@&@%�@%@%�h@%p�@%O�@%/@%V@$�@$�@$I�@#�m@#C�@#"�@#@"�H@"��@"��@"^5@!��@!�^@!��@!G�@!�@ ��@ �9@   @�w@�@��@|�@l�@K�@�R@E�@{@�T@V@��@z�@Z@(�@�
@dZ@�H@��@�\@��@x�@X@&�@��@��@�@  @l�@+@
=@��@��@�y@�+@V@E�@5?@5?@5?@$�@$�@5?@$�@$�@{@{@{@�T@�@�j@��@�D@Z@ƨ@�!@��@��@��@��@�7@X@&�@%@��@��@�9@�u@1'@b@�@|�@K�@��@5?@��@/@�/@z�@9X@�@�m@�
@��@�@S�@33@
�@
�H@
��@
~�@
n�@
^5@
^5@
-@
�@	��@	�^@	��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A❲A��A��A��A��A��A��A��A��A��A��A��A◍A�I�A��A۬A֛�A�%A�M�A�%A�z�A�"�A�K�A��A�x�A���A�l�A��A���A�K�A��;A�A�%A��PA��A�Q�A�(�A���A��A�+A��7A��
A�ZA��FA���A���A��A��7A�r�A�=qA��yA���A�$�Av�DAq�An=qAh�Ae�PAa��A^�`A]��A\JAY�AW�TAU?}AQ��AOp�AN�+AM�AKS�AH�HAF�+AD��AB^5A;K�A:��A=/A>$�A=�^A="�A<��A<-A;dZA:-A8A�A6�/A6�+A6VA5�wA3\)A2�A2^5A1�A1`BA0��A0M�A0 �A/��A.��A-��A-|�A-hsA-G�A-��A-"�A,��A,�RA,��A,�+A,^5A,�A+�A+�
A+��A+�7A+XA*�jA)��A)|�A)%A(��A(z�A'�A'�A'oA&�HA&VA%��A%ƨA%��A%K�A%&�A$��A$�9A$bA#��A#��A#S�A#�A"�A"��A"��A"��A"��A"jA"E�A"=qA!��A!G�A!/A!�A!VA ��A ȴA v�A �A�A�;A�FA��A�uAI�A$�A��A\)A�AA�jAA�A�;AƨAA��A�hAl�AG�A�A%A�`A�A�\AQ�A�mAXA��A��A�jA�!AjAAhsA33A�A�DAI�A�A\)A+A�AVA��AI�AJA�
A�hA7LA��A�/A��A �A1A�
A�7A`BA"�A�+A�A�7AVA�!A^5A�AM�At�A
�A
�9A
�A	��A	�7A	K�A��A�HA��AZA �A�hA33A��AjA��Ax�AO�A�A�!A�A�!AA�A$�A �A  A�A33A
=A �uA �@���@�t�@���@�$�@��h@�j@���@�|�@�K�@��R@�$�@��7@�7L@���@�1@���@���@��@��@�`B@�G�@��@���@�+@���@�V@�?}@��D@�Z@�1@@���@�n�@�{@�G�@�@�ƨ@�~�@�@�@�7L@�z�@��;@��@�V@��@�1@���@�F@�
=@�@�@�G�@�Ĝ@�(�@���@ߝ�@��H@�~�@�ff@�^5@�E�@ݡ�@ܴ9@���@��@�@���@ٲ-@���@�\)@թ�@�V@�z�@�\)@ҸR@Ѳ-@ѩ�@�&�@�Ĝ@� �@ϝ�@�S�@��@��@�=q@��@͙�@�O�@��/@�j@���@ˍP@�\)@�+@���@���@ʗ�@��@��`@ȣ�@��m@Ǿw@ǝ�@���@��y@�$�@�x�@���@�j@��m@��@�$�@���@���@�r�@���@�+@�M�@��@��@��`@�1@�ƨ@�C�@�ȴ@��+@�-@��@�hs@�/@��/@��D@�Q�@��@���@��@���@���@�
=@��H@�ff@�`B@���@��j@��@���@�j@��
@�o@���@��R@�v�@�ff@�E�@�-@��@�O�@���@�1@�K�@�+@�o@�~�@��#@�`B@��@���@�z�@�9X@�(�@��@���@�33@���@���@�n�@�5?@��#@��h@�%@���@�(�@���@�n�@��@���@�O�@�V@��@�bN@�9X@��
@�@�n�@��@�@�hs@��`@���@�I�@��;@���@�dZ@�o@���@�~�@�ff@�=q@�@�@��@�`B@���@�j@�1@��@�o@��!@��\@�5?@���@��7@�V@��`@�Ĝ@��j@��@���@��;@�S�@��@�-@��#@���@��@�/@�V@���@�9X@�1@��m@��F@�\)@��H@���@���@��R@��+@��-@�`B@�`B@��@��9@��@�j@�I�@��m@��@�v�@�5?@���@���@�G�@��@���@��9@��@�j@�bN@�1'@�K�@���@�=q@�J@��@���@�`B@�&�@��@�%@��@� �@��;@��F@�|�@�;d@�@���@��@���@���@�~�@�ff@�J@�O�@�7L@�V@���@���@��@��u@��@�z�@�r�@�bN@�1'@�(�@�1@��w@��P@�dZ@�K�@�33@�
=@���@�ff@�M�@�=q@�-@��@�@�`B@�X@�O�@�/@���@��`@��@�(�@�  @�w@�P@\)@
=@~ȴ@~ff@~{@}�h@}�@|�@|z�@|9X@{��@{�@z�!@y�#@y7L@x��@x��@x��@x�9@xb@v��@vv�@v5?@u��@u�h@s�
@sC�@s@r�@r��@r��@r�!@r^5@q�#@q&�@p1'@o�w@o
=@n�+@n$�@m@m`B@k�m@k��@kS�@j�!@i��@i%@hĜ@hbN@g�P@fff@e�T@e�@d�j@cƨ@co@b��@bn�@b^5@bM�@b-@b�@bJ@a��@aX@`��@_�;@_�@^��@^��@^{@]p�@\�j@[�m@[o@Z�!@Z�@Y�@Y��@YX@X��@XQ�@W�;@W�;@W�w@W|�@W�@V��@U��@T�/@T��@T��@T��@T(�@S�
@S��@SS�@SC�@S"�@S@R�H@R��@R�!@R��@R��@R~�@RM�@R�@Q��@Q%@Pb@O�w@O
=@N�y@N��@Nff@N@M��@Mp�@M�@L�@L��@LI�@L(�@K�m@K�@K33@JM�@I�@I�7@I7L@H��@H�u@H1'@H1'@G�;@G�P@G\)@G;d@Fȴ@FE�@F@E��@EO�@E/@D�/@D��@Dz�@D9X@D1@C��@CC�@C"�@B��@B�!@Bn�@B-@A��@A�@A�^@A�7@A��@A�^@@��@?�@?�@?�P@?|�@?\)@?;d@?�@>�@>ȴ@>�R@>��@>�+@>5?@=�-@=O�@<��@<�@<j@;�m@;S�@;@:��@:��@:��@:n�@9�#@9&�@8�`@8�@8bN@81'@8b@7��@7�@6�@6v�@6$�@5�T@5�-@5�h@5?}@4�@4��@49X@4�@3�@3S�@3C�@3C�@3@2��@2��@2~�@2J@1�7@0Ĝ@0�@0r�@01'@0 �@/�@/�w@/�@/�@/�@/��@/�P@/l�@/l�@/l�@/\)@/;d@.ȴ@.��@.v�@.V@.5?@.$�@-p�@-V@,�/@,�@,z�@,(�@+ƨ@+o@*��@*n�@*M�@*�@)��@)��@)��@)��@)x�@)hs@)7L@(��@(Ĝ@(��@(Q�@(A�@(1'@'�@'�w@'\)@';d@';d@';d@'+@'�@&��@&��@&��@&�y@&�y@&�@&��@&v�@&E�@&$�@&{@&@%�@%@%�h@%p�@%O�@%/@%V@$�@$�@$I�@#�m@#C�@#"�@#@"�H@"��@"��@"^5@!��@!�^@!��@!G�@!�@ ��@ �9@   @�w@�@��@|�@l�@K�@�R@E�@{@�T@V@��@z�@Z@(�@�
@dZ@�H@��@�\@��@x�@X@&�@��@��@�@  @l�@+@
=@��@��@�y@�+@V@E�@5?@5?@5?@$�@$�@5?@$�@$�@{@{@{@�T@�@�j@��@�D@Z@ƨ@�!@��@��@��@��@�7@X@&�@%@��@��@�9@�u@1'@b@�@|�@K�@��@5?@��@/@�/@z�@9X@�@�m@�
@��@�@S�@33@
�@
�H@
��@
~�@
n�@
^5@
^5@
-@
�@	��@	�^@	��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B��B��B��B��B��B��B��B��B��B��B�FB��B
JB
�-B
��B
�B
�jB
�B
�uB
�{B
�bB
�%B
�dBPBQ�Bp�B��B�-B�oB�B�B�'B�!B��B�bB�VB�B�B�B�B�B�hB��B�JB~�Bk�BA�B
�B
�B
T�B	�5B	�^B	�B	�uB	�B	v�B	m�B	gmB	cTB	\)B	[#B	YB	gmB	�B	�JB	�\B	��B	��B	�{B	�B	x�B	bNB	��B	�B
PB
#�B
2-B
9XB
F�B
W
B
_;B
o�B
z�B
�%B
�=B
�DB
�%B
�B
�=B
�bB
�hB
�hB
��B
��B
��B
��B
��B
��B
��B
��B
�'B
�3B
�-B
�9B
�?B
�LB
�LB
�XB
�XB
�XB
�XB
�XB
�^B
�jB
�dB
�jB
�qB
�jB
�qB
�jB
�jB
�dB
�qB
�qB
�qB
�wB
�wB
�wB
�wB
�qB
�jB
�^B
�XB
�XB
�^B
�^B
�^B
�^B
�wB
�qB
�qB
�qB
�qB
�jB
�jB
�^B
�^B
�^B
�XB
�XB
�RB
�LB
�?B
�9B
�9B
�-B
�!B
�B
�B
�B
�B
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�{B
�{B
�oB
�hB
�hB
�bB
�\B
�VB
�\B
�VB
�PB
�DB
�=B
�=B
�=B
�1B
�+B
�+B
�%B
�B
�B
�B
~�B
|�B
z�B
x�B
v�B
s�B
o�B
n�B
k�B
jB
iyB
hsB
ffB
ffB
e`B
e`B
dZB
cTB
bNB
aHB
`BB
_;B
_;B
\)B
\)B
[#B
[#B
YB
XB
T�B
S�B
S�B
R�B
R�B
Q�B
P�B
O�B
N�B
L�B
L�B
K�B
J�B
J�B
I�B
G�B
F�B
F�B
F�B
E�B
D�B
C�B
B�B
B�B
A�B
@�B
@�B
>wB
>wB
>wB
>wB
=qB
=qB
<jB
;dB
:^B
9XB
8RB
8RB
7LB
7LB
6FB
5?B
5?B
49B
49B
33B
2-B
1'B
1'B
1'B
0!B
0!B
0!B
/B
/B
.B
.B
.B
.B
-B
-B
-B
,B
,B
+B
+B
)�B
)�B
)�B
(�B
(�B
(�B
&�B
%�B
%�B
%�B
$�B
#�B
#�B
$�B
"�B
!�B
!�B
!�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
 �B
 �B
 �B
 �B
 �B
 �B
 �B
!�B
"�B
"�B
"�B
"�B
!�B
!�B
 �B
 �B
 �B
!�B
!�B
!�B
#�B
#�B
$�B
%�B
%�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
)�B
)�B
+B
)�B
+B
+B
+B
,B
,B
,B
,B
,B
-B
,B
-B
-B
-B
,B
,B
,B
,B
-B
,B
-B
-B
-B
-B
-B
-B
.B
.B
.B
.B
.B
.B
-B
.B
.B
.B
.B
.B
.B
.B
/B
/B
0!B
0!B
1'B
1'B
1'B
2-B
33B
33B
2-B
2-B
2-B
49B
49B
49B
49B
5?B
5?B
5?B
5?B
6FB
7LB
7LB
7LB
8RB
8RB
9XB
8RB
8RB
9XB
8RB
8RB
8RB
8RB
9XB
:^B
:^B
;dB
;dB
;dB
;dB
;dB
;dB
<jB
<jB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
@�B
@�B
?}B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
L�B
L�B
N�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
S�B
S�B
S�B
S�B
T�B
VB
T�B
T�B
VB
VB
W
B
W
B
W
B
XB
XB
XB
XB
XB
XB
XB
YB
YB
YB
YB
YB
ZB
ZB
ZB
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
aHB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
ffB
ffB
ffB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
k�B
jB
jB
iyB
jB
jB
jB
jB
k�B
l�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
m�B
m�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�%B
�+B
�+B
�1B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�DB
�DB
�DB
�DB
�DB
�JB
�PB
�VB
�VB
�VB
�VB
�VB
�VB
�\B
�\B
�\B
�\B
�\B
�\B
�bB
�\B
�bB
�hB
�hB
�hB
�oB
�oB
�uB
�uB
�{B
�{B
�{B
�{B
�{B
�{B
�{B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B��B��B��B��B��B��B��B��B��B��B�FB��B
JB
�-B
��B
�B
�jB
�B
�uB
�{B
�bB
�%B
�dBPBQ�Bp�B��B�-B�oB�B�B�'B�!B��B�bB�VB�B�B�B�B�B�hB��B�JB~�Bk�BA�B
�B
�B
T�B	�5B	�^B	�B	�uB	�B	v�B	m�B	gmB	cTB	\)B	[#B	YB	gmB	�B	�JB	�\B	��B	��B	�{B	�B	x�B	bNB	��B	�B
PB
#�B
2-B
9XB
F�B
W
B
_;B
o�B
z�B
�%B
�=B
�DB
�%B
�B
�=B
�bB
�hB
�hB
��B
��B
��B
��B
��B
��B
��B
��B
�'B
�3B
�-B
�9B
�?B
�LB
�LB
�XB
�XB
�XB
�XB
�XB
�^B
�jB
�dB
�jB
�qB
�jB
�qB
�jB
�jB
�dB
�qB
�qB
�qB
�wB
�wB
�wB
�wB
�qB
�jB
�^B
�XB
�XB
�^B
�^B
�^B
�^B
�wB
�qB
�qB
�qB
�qB
�jB
�jB
�^B
�^B
�^B
�XB
�XB
�RB
�LB
�?B
�9B
�9B
�-B
�!B
�B
�B
�B
�B
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�{B
�{B
�oB
�hB
�hB
�bB
�\B
�VB
�\B
�VB
�PB
�DB
�=B
�=B
�=B
�1B
�+B
�+B
�%B
�B
�B
�B
~�B
|�B
z�B
x�B
v�B
s�B
o�B
n�B
k�B
jB
iyB
hsB
ffB
ffB
e`B
e`B
dZB
cTB
bNB
aHB
`BB
_;B
_;B
\)B
\)B
[#B
[#B
YB
XB
T�B
S�B
S�B
R�B
R�B
Q�B
P�B
O�B
N�B
L�B
L�B
K�B
J�B
J�B
I�B
G�B
F�B
F�B
F�B
E�B
D�B
C�B
B�B
B�B
A�B
@�B
@�B
>wB
>wB
>wB
>wB
=qB
=qB
<jB
;dB
:^B
9XB
8RB
8RB
7LB
7LB
6FB
5?B
5?B
49B
49B
33B
2-B
1'B
1'B
1'B
0!B
0!B
0!B
/B
/B
.B
.B
.B
.B
-B
-B
-B
,B
,B
+B
+B
)�B
)�B
)�B
(�B
(�B
(�B
&�B
%�B
%�B
%�B
$�B
#�B
#�B
$�B
"�B
!�B
!�B
!�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
 �B
 �B
 �B
 �B
 �B
 �B
 �B
!�B
"�B
"�B
"�B
"�B
!�B
!�B
 �B
 �B
 �B
!�B
!�B
!�B
#�B
#�B
$�B
%�B
%�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
)�B
)�B
+B
)�B
+B
+B
+B
,B
,B
,B
,B
,B
-B
,B
-B
-B
-B
,B
,B
,B
,B
-B
,B
-B
-B
-B
-B
-B
-B
.B
.B
.B
.B
.B
.B
-B
.B
.B
.B
.B
.B
.B
.B
/B
/B
0!B
0!B
1'B
1'B
1'B
2-B
33B
33B
2-B
2-B
2-B
49B
49B
49B
49B
5?B
5?B
5?B
5?B
6FB
7LB
7LB
7LB
8RB
8RB
9XB
8RB
8RB
9XB
8RB
8RB
8RB
8RB
9XB
:^B
:^B
;dB
;dB
;dB
;dB
;dB
;dB
<jB
<jB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
@�B
@�B
?}B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
L�B
L�B
N�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
S�B
S�B
S�B
S�B
T�B
VB
T�B
T�B
VB
VB
W
B
W
B
W
B
XB
XB
XB
XB
XB
XB
XB
YB
YB
YB
YB
YB
ZB
ZB
ZB
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
aHB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
ffB
ffB
ffB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
k�B
jB
jB
iyB
jB
jB
jB
jB
k�B
l�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
m�B
m�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�%B
�+B
�+B
�1B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�DB
�DB
�DB
�DB
�DB
�JB
�PB
�VB
�VB
�VB
�VB
�VB
�VB
�\B
�\B
�\B
�\B
�\B
�\B
�bB
�\B
�bB
�hB
�hB
�hB
�oB
�oB
�uB
�uB
�{B
�{B
�{B
�{B
�{B
�{B
�{B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.33 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20211002090051                              AO  ARCAADJP                                                                    20211002090051    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20211002090051  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20211002090051  QCF$                G�O�G�O�G�O�0               