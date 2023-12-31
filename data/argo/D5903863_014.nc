CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       d2014-07-21T23:38:51Z creation; 2014-07-21T23:38:51Z updated; 2015-09-28T12:13:17Z converted from 3.0   
references        (http://www.argodatamgt.org/Documentation   comment           user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7    PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7`   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8    PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8$   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8D   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8d   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~       axis      T           8h   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8p   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~            8t   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8|   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        l  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \\   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  `8   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  o�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �     TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  �4   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  �0   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ۜ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �H   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �X   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �\   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �l   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �p   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �t   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20140721233851  20170523133325  5903863 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               A   AO  4298_0127_014                   2C  D   NAVIS_A                         0127                            120111                          863 @�B�$��1   @�B��o��@5~��"���d]V�1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      A   A   A   @���@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@�fDA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� DpfDp� DqfDq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�3D�C3DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D��fD�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @~|@�=q@�p�A�RA:�RAZ�RAz�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B�B�B�B&�B.�B6�B>�BF�BN�BV�B^�Bf�Bn�Bv�B~�B�W
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
C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D j�D ��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��D	j�D	��D
j�D
��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��D j�D ��D!j�D!��D"j�D"��D#j�D#��D$j�D$��D%j�D%��D&j�D&��D'j�D'��D(j�D(��D)j�D)��D*j�D*��D+j�D+��D,j�D,��D-j�D-��D.j�D.��D/j�D/��D0j�D0��D1j�D1��D2j�D2��D3j�D3��D4j�D4��D5j�D5��D6j�D6��D7j�D7��D8j�D8��D9j�D9��D:j�D:��D;j�D;��D<j�D<��D=j�D=��D>j�D>��D?j�D?��D@qGD@��DAj�DA��DBj�DB��DCj�DC��DDj�DD��DEj�DE��DFj�DF��DGj�DG��DHj�DH��DIj�DI��DJj�DJ��DKj�DK��DLj�DL��DMj�DM��DNj�DN��DOj�DO��DPj�DP��DQj�DQ��DRj�DR��DSj�DS��DTj�DT��DUj�DU��DVj�DV��DWj�DW��DXj�DX��DYj�DY��DZj�DZ��D[j�D[��D\j�D\��D]j�D]��D^j�D^��D_j�D_��D`j�D`��Daj�Da��Dbj�Db��Dcj�Dc��Ddj�Dd��Dej�De��Dfj�Df��Dgj�Dg��Dhj�Dh��Dij�Di��Djj�Dj��Dkj�Dk��Dlj�Dl��Dmj�Dm��Dnj�Dn��Doj�Do�GDpj�Dp�GDqj�Dq��Drj�Dr��Dsj�Ds��Dtj�Dt��Duj�Du��Dvj�Dv��Dwj�Dw��Dxj�Dx��Dyj�Dy��Dzj�Dz��D{j�D{��D|j�D|��D}j�D}��D~j�D~��Dj�D��D�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�2>D�r>D��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqDµqD��qD�5qD�uqDõqD��qD�5qD�uqDĵqD��qD�5qD�uqDŵqD��qD�5qD�uqDƵqD��qD�5qD�uqDǵqD��qD�5qD�uqDȵqD��qD�5qD�uqDɵqD��qD�5qD�uqDʵqD��qD�5qD�uqD˵qD��qD�5qD�uqD̵qD��qD�5qD�uqD͵qD��qD�5qD�uqDεqD��qD�5qD�uqDϵqD��qD�5qD�uqDеqD��qD�5qD�uqDѵqD��qD�5qD�uqDҵqD��qD�5qD�uqDӵqD���D�8�D�uqDԵqD��qD�5qD�uqDյqD��qD�5qD�uqDֵqD��qD�5qD�uqD׵qD��qD�5qD�uqDصqD��qD�5qD�uqDٵqD��qD�5qD�uqDڵqD��qD�5qD�uqD۵qD��qD�5qD�uqDܵqD��qD�5qD�uqDݵqD��qD�5qD�uqD޵qD��qD�5qD�uqDߵqD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD��qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD���D��q111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A�{A�"�A�+A�-A�-A�/A�/A�/A�1'A�1'A�1'A�33A�1'A�33A�5?A�5?A�5?A�5?A�5?A�7LA�7LA�"�A�VA��`AȺ^A�O�AŸRAţ�A�bAã�A�/A��A�XA��;A�A�"�A�1A���A�JA���A�1A�JA���A�VA��`A�Q�A�A�M�A��9A���A��+A���A�n�A��A�33A��RA��;A� �A��wA���A�x�A�t�A���A�v�A�A��/A�~�A�VA��jA�S�A��7A�"�A�^5A�A�A�(�A��!A�VA���A�Q�A���A���A���A��;A�`BA���A�l�A�1'A��A��A�;dA�v�A���A�M�A�A�A�bNA�A�G�A+A}+Az~�AyAw��Av��Av-Au��At��As�7Ar�Ar��Arn�Ar-Aq\)Ap�An��Al��Alr�AlZAkO�Ail�AgO�Ad^5Ac��AcO�Ab�`Ab�\Aa��Aa/A`�9A_?}A]��AZ�AX�DAU�7AQ�
AP�RAOS�ANjAM�#AK�AK;dAJv�AI�wAI;dAG��AG?}AF~�AE�A@��A>��A>�A=A<jA:��A9��A97LA8�HA8�+A7�A7t�A6�A5\)A4��A2�`A1�hA1C�A1?}A1"�A0��A0{A.��A,ȴA+p�A)O�A(�A(��A(5?A'�#A'G�A& �A$�9A#�A"1A �RA�7A\)A&�A(�AS�AVA�wA��A�A+A�+Al�Av�A1A?}AĜA�DAA�!AAjA��AK�A�yA�A�A�+A�A
r�A
(�A	l�A-A�TA`BAA�A�A�wAC�A��A�TA �@��@�-@�b@���@�K�@�-@�%@�z�@�w@���@�O�@�Z@��@�9@�-@���@�  @��@◍@���@�~�@�V@ם�@�5?@ӍP@��@�z�@Ο�@��/@�K�@�"�@�$�@�-@�@�@�J@�@��T@�7L@ȃ@�9X@��
@�C�@�33@�-@�t�@��u@��^@�r�@�Q�@�A�@�1@�"�@��@�~�@�K�@�|�@�|�@�t�@�t�@��@��\@��+@�@�I�@���@�l�@�"�@���@�$�@�Q�@�v�@���@�/@�bN@��m@���@�v�@��@��P@�|�@�K�@�"�@�o@�~�@��h@�?}@�%@�j@��H@�$�@��h@�p�@���@��P@�^5@��@�@��u@���@�"�@�ff@��@��^@�O�@�%@���@�Q�@�dZ@�33@�"�@��@���@���@�ȴ@��+@�M�@�M�@�V@�^5@�n�@�^5@�@��D@�  @���@���@��
@��@�|�@�+@�^5@�M�@�$�@��@�p�@�(�@��@�t�@�+@���@���@���@���@�E�@���@�G�@���@��@�z�@�1'@��;@��F@���@�l�@�S�@�S�@�
=@���@��@��@��y@���@���@�M�@�$�@�@�@��h@��7@�`B@��@�Ĝ@��@��D@���@��@�r�@���@�|�@�t�@��@��@�\)@��@��y@���@�=q@��@���@��h@�/@�&�@�&�@��@��`@��@�1'@���@�ƨ@���@�S�@�33@��y@���@��\@�E�@��T@��^@���@��7@�X@�?}@��@�%@���@���@���@�Q�@���@���@���@���@�\)@�dZ@�o@�^5@��@��@�@��h@�X@��@��@���@�Ĝ@���@��@�b@��
@�ƨ@���@��P@�t�@�33@�
=@���@�E�@�-@��@�J@��@��T@��#@�@���@��@�hs@�G�@��@���@��@�Z@�b@�  @��@��;@��w@�\)@�;d@�@�ȴ@���@��+@�~�@�5?@���@���@���@���@��7@�X@��@��9@��@�bN@�(�@�;@�@�P@l�@~ff@}�T@}�h@}O�@}?}@}�@|��@|�@|�j@|z�@|Z@|(�@|�@{�F@z��@z^5@zM�@y��@yX@x��@x �@w��@w�P@w|�@w;d@w
=@v�y@v�R@vE�@u�h@t�@t�@sdZ@s@r��@r�!@r��@r^5@q�#@qx�@p��@p��@p�@pr�@p1'@o�@o|�@o+@nv�@nv�@n@m�@mp�@l�/@lz�@l�@k�F@k��@k�@kS�@j�@jM�@i��@i�7@ix�@i7L@h�`@hbN@h �@g�;@g�;@g��@g�P@g+@f�R@fff@fV@f$�@e�@eV@d�/@dj@dI�@d(�@c�
@c��@cdZ@b��@b-@a��@ax�@a&�@`�@_�;@_l�@_K�@_�@^5?@]O�@]V@\��@[�@Z��@Zn�@Z^5@Z^5@Y��@Y�7@Y�7@YG�@XĜ@W�;@W�P@V�y@Vff@U�T@UO�@T��@T1@SS�@So@R~�@R�@Q��@P��@P�9@P��@Pr�@PbN@O�;@O;d@Nȴ@N��@Nv�@Nff@M��@M�@M?}@M/@M/@M/@M/@M/@MV@L�@L��@LI�@K�m@K�F@KS�@J�H@J��@J~�@J^5@J-@I�@I��@I�^@I��@Ihs@H�`@H��@HĜ@H�9@H�@Hr�@HQ�@H �@H  @G�;@G��@G�@GK�@G�@F��@F��@F��@F�y@F�@Fȴ@F��@F�+@FV@F{@E�h@E`B@E�@D�@D��@D��@D��@D�j@C��@Cƨ@Cƨ@C�F@C�@CC�@C33@B�@B�!@B=q@A�^@A�^@AG�@@��@@��@@��@@ �@?��@?K�@?;d@?;d@?;d@?;d@?+@?+@?
=@>�@>�@>ȴ@>��@>5?@=`B@=/@<�@<I�@<1@;�
@;�F@;t�@;C�@;33@;"�@;o@:�H@:��@:^5@9��@9��@9G�@8��@8�u@8Q�@8 �@7�;@7�@7|�@7|�@7K�@6�@6��@6$�@5@5��@5��@5@5�h@5O�@5�@5V@4�@4I�@3ƨ@3t�@3@2�@2��@2n�@2M�@2=q@1�#@17L@1�@1�@1%@0�`@0�@0 �@0  @/�@/�;@/|�@/+@.��@.ff@.5?@.$�@-��@-/@-V@-V@-V@,�@,I�@+��@+ƨ@+��@+dZ@+33@+@*�@*��@*�\@*-@*J@)��@)��@)G�@)&�@)&�@)�@(�u@(r�@(A�@(b@'��@'K�@'
=@&�y@&ȴ@&�R@&��@&ff@%��@%�h@%�@%p�@%?}@%�@%V@$��@$��@$�j@$9X@$(�@#��@#C�@#33@#33@#o@"��@"n�@"�@!�@!�#@!�^@!��@!hs@!X@!G�@!7L@!7L@!%@ ��@ ��@ bN@ bN@ A�@  �@ b@�w@l�@K�@;d@;d@+@��@�R@�R@��@v�@ff@5?@$�@�@�-@/@�@z�@1@�
@�F@"�@�@�H@�@o@�H@�!@~�@=q@J@�@��@��@�^@�7@x�@hs@X@%@�9@�@�@�@�@�@ �@�@�;@��@��@��@�@�@\)@�@ȴ@�R@��@$�@@@p�@?}@�@V@��@�@��@�j@��@z�@j@Z@I�@9X@1@��@��@�
@ƨ@��@dZ@C�@"�@@��@�\@-@J@�@�#@�#@�^@G�@�@%@�`@Ĝ@�u@A�@  @��@;d@
=@��@V@E�@5?111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A�{A�"�A�+A�-A�-A�/A�/A�/A�1'A�1'A�1'A�33A�1'A�33A�5?A�5?A�5?A�5?A�5?A�7LA�7LA�"�A�VA��`AȺ^A�O�AŸRAţ�A�bAã�A�/A��A�XA��;A�A�"�A�1A���A�JA���A�1A�JA���A�VA��`A�Q�A�A�M�A��9A���A��+A���A�n�A��A�33A��RA��;A� �A��wA���A�x�A�t�A���A�v�A�A��/A�~�A�VA��jA�S�A��7A�"�A�^5A�A�A�(�A��!A�VA���A�Q�A���A���A���A��;A�`BA���A�l�A�1'A��A��A�;dA�v�A���A�M�A�A�A�bNA�A�G�A+A}+Az~�AyAw��Av��Av-Au��At��As�7Ar�Ar��Arn�Ar-Aq\)Ap�An��Al��Alr�AlZAkO�Ail�AgO�Ad^5Ac��AcO�Ab�`Ab�\Aa��Aa/A`�9A_?}A]��AZ�AX�DAU�7AQ�
AP�RAOS�ANjAM�#AK�AK;dAJv�AI�wAI;dAG��AG?}AF~�AE�A@��A>��A>�A=A<jA:��A9��A97LA8�HA8�+A7�A7t�A6�A5\)A4��A2�`A1�hA1C�A1?}A1"�A0��A0{A.��A,ȴA+p�A)O�A(�A(��A(5?A'�#A'G�A& �A$�9A#�A"1A �RA�7A\)A&�A(�AS�AVA�wA��A�A+A�+Al�Av�A1A?}AĜA�DAA�!AAjA��AK�A�yA�A�A�+A�A
r�A
(�A	l�A-A�TA`BAA�A�A�wAC�A��A�TA �@��@�-@�b@���@�K�@�-@�%@�z�@�w@���@�O�@�Z@��@�9@�-@���@�  @��@◍@���@�~�@�V@ם�@�5?@ӍP@��@�z�@Ο�@��/@�K�@�"�@�$�@�-@�@�@�J@�@��T@�7L@ȃ@�9X@��
@�C�@�33@�-@�t�@��u@��^@�r�@�Q�@�A�@�1@�"�@��@�~�@�K�@�|�@�|�@�t�@�t�@��@��\@��+@�@�I�@���@�l�@�"�@���@�$�@�Q�@�v�@���@�/@�bN@��m@���@�v�@��@��P@�|�@�K�@�"�@�o@�~�@��h@�?}@�%@�j@��H@�$�@��h@�p�@���@��P@�^5@��@�@��u@���@�"�@�ff@��@��^@�O�@�%@���@�Q�@�dZ@�33@�"�@��@���@���@�ȴ@��+@�M�@�M�@�V@�^5@�n�@�^5@�@��D@�  @���@���@��
@��@�|�@�+@�^5@�M�@�$�@��@�p�@�(�@��@�t�@�+@���@���@���@���@�E�@���@�G�@���@��@�z�@�1'@��;@��F@���@�l�@�S�@�S�@�
=@���@��@��@��y@���@���@�M�@�$�@�@�@��h@��7@�`B@��@�Ĝ@��@��D@���@��@�r�@���@�|�@�t�@��@��@�\)@��@��y@���@�=q@��@���@��h@�/@�&�@�&�@��@��`@��@�1'@���@�ƨ@���@�S�@�33@��y@���@��\@�E�@��T@��^@���@��7@�X@�?}@��@�%@���@���@���@�Q�@���@���@���@���@�\)@�dZ@�o@�^5@��@��@�@��h@�X@��@��@���@�Ĝ@���@��@�b@��
@�ƨ@���@��P@�t�@�33@�
=@���@�E�@�-@��@�J@��@��T@��#@�@���@��@�hs@�G�@��@���@��@�Z@�b@�  @��@��;@��w@�\)@�;d@�@�ȴ@���@��+@�~�@�5?@���@���@���@���@��7@�X@��@��9@��@�bN@�(�@�;@�@�P@l�@~ff@}�T@}�h@}O�@}?}@}�@|��@|�@|�j@|z�@|Z@|(�@|�@{�F@z��@z^5@zM�@y��@yX@x��@x �@w��@w�P@w|�@w;d@w
=@v�y@v�R@vE�@u�h@t�@t�@sdZ@s@r��@r�!@r��@r^5@q�#@qx�@p��@p��@p�@pr�@p1'@o�@o|�@o+@nv�@nv�@n@m�@mp�@l�/@lz�@l�@k�F@k��@k�@kS�@j�@jM�@i��@i�7@ix�@i7L@h�`@hbN@h �@g�;@g�;@g��@g�P@g+@f�R@fff@fV@f$�@e�@eV@d�/@dj@dI�@d(�@c�
@c��@cdZ@b��@b-@a��@ax�@a&�@`�@_�;@_l�@_K�@_�@^5?@]O�@]V@\��@[�@Z��@Zn�@Z^5@Z^5@Y��@Y�7@Y�7@YG�@XĜ@W�;@W�P@V�y@Vff@U�T@UO�@T��@T1@SS�@So@R~�@R�@Q��@P��@P�9@P��@Pr�@PbN@O�;@O;d@Nȴ@N��@Nv�@Nff@M��@M�@M?}@M/@M/@M/@M/@M/@MV@L�@L��@LI�@K�m@K�F@KS�@J�H@J��@J~�@J^5@J-@I�@I��@I�^@I��@Ihs@H�`@H��@HĜ@H�9@H�@Hr�@HQ�@H �@H  @G�;@G��@G�@GK�@G�@F��@F��@F��@F�y@F�@Fȴ@F��@F�+@FV@F{@E�h@E`B@E�@D�@D��@D��@D��@D�j@C��@Cƨ@Cƨ@C�F@C�@CC�@C33@B�@B�!@B=q@A�^@A�^@AG�@@��@@��@@��@@ �@?��@?K�@?;d@?;d@?;d@?;d@?+@?+@?
=@>�@>�@>ȴ@>��@>5?@=`B@=/@<�@<I�@<1@;�
@;�F@;t�@;C�@;33@;"�@;o@:�H@:��@:^5@9��@9��@9G�@8��@8�u@8Q�@8 �@7�;@7�@7|�@7|�@7K�@6�@6��@6$�@5@5��@5��@5@5�h@5O�@5�@5V@4�@4I�@3ƨ@3t�@3@2�@2��@2n�@2M�@2=q@1�#@17L@1�@1�@1%@0�`@0�@0 �@0  @/�@/�;@/|�@/+@.��@.ff@.5?@.$�@-��@-/@-V@-V@-V@,�@,I�@+��@+ƨ@+��@+dZ@+33@+@*�@*��@*�\@*-@*J@)��@)��@)G�@)&�@)&�@)�@(�u@(r�@(A�@(b@'��@'K�@'
=@&�y@&ȴ@&�R@&��@&ff@%��@%�h@%�@%p�@%?}@%�@%V@$��@$��@$�j@$9X@$(�@#��@#C�@#33@#33@#o@"��@"n�@"�@!�@!�#@!�^@!��@!hs@!X@!G�@!7L@!7L@!%@ ��@ ��@ bN@ bN@ A�@  �@ b@�w@l�@K�@;d@;d@+@��@�R@�R@��@v�@ff@5?@$�@�@�-@/@�@z�@1@�
@�F@"�@�@�H@�@o@�H@�!@~�@=q@J@�@��@��@�^@�7@x�@hs@X@%@�9@�@�@�@�@�@ �@�@�;@��@��@��@�@�@\)@�@ȴ@�R@��@$�@@@p�@?}@�@V@��@�@��@�j@��@z�@j@Z@I�@9X@1@��@��@�
@ƨ@��@dZ@C�@"�@@��@�\@-@J@�@�#@�#@�^@G�@�@%@�`@Ĝ@�u@A�@  @��@;d@
=@��@V@E�@5?111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBcTBcTBcTBbNBcTBcTBbNBcTBbNBbNBcTBcTBcTBcTBcTBcTBcTBcTBcTBcTBcTBbNBbNBbNBaHBaHB`BB^5B\)BQ�B>wB,B#�B�B�B�B�B�B�BVB	7BBBBBBBBB  B��B��B�B�B�B�BB��BɺBǮB�}B�FB�B�bBy�BjBN�B=qB7LB.B�BPB��B�B�ZB��BĜB�!B��By�BaHBA�B.B#�BuB
��B
�B
�B
�B
�fB
�HB
�#B
��B
��B
�!B
��B
�hB
~�B
s�B
hsB
ZB
G�B
=qB
5?B
.B
+B
&�B
!�B
�B
{B
oB
hB
\B
	7B
B	��B	�B	�yB	�mB	�5B	��B	ŢB	�LB	�'B	�B	�B	��B	��B	��B	��B	�bB	�B	v�B	q�B	gmB	[#B	VB	P�B	K�B	E�B	B�B	?}B	=qB	:^B	7LB	2-B	.B	'�B	�B	bB		7B	%B	B��B��B�B�B�B�B�B�B�B�mB�TB�/B�B�
B�
B�B��B��BɺBÖB�wB�^B�RB�LB�?B�9B�'B�B��B��B��B��B��B��B��B�uB�hB�bB�VB�JB�DB�7B�+B�B�B~�B{�Bx�Bv�Bt�Bs�Bo�Bl�BjBhsBgmBffBdZBbNB_;B\)BZBXBVBT�BR�BP�BO�BN�BM�BK�BH�BE�BB�B?}B>wB=qB=qB=qB<jB<jB<jB;dB;dB:^B;dB:^B<jB;dB<jB;dB8RB<jBA�BA�B@�B@�B@�B@�BC�BF�BH�BJ�BJ�BP�BVBXB\)B_;B_;BaHBgmBm�Bm�Bn�Br�B~�B|�Bt�Bl�Bk�Bl�Bo�Bq�Bs�Bq�Bv�Bw�B|�B~�B�B�B�B�B�B�B�DB�uB��B��B�B�-B�-B�9B�?B�FB�RB�^B�^B�^B�^B�qB�}B��BÖBĜBĜBƨBɺB��B��B��B��B�B�;B�BB�HB�HB�TB�`B�`B�yB�B�B�B�B�B��B��B��B��B��B��B	  B	B	B		7B	JB	VB	bB	hB	oB	oB	�B	�B	�B	uB	hB	oB	�B	�B	�B	�B	�B	�B	%�B	%�B	'�B	'�B	'�B	)�B	-B	.B	/B	49B	8RB	:^B	;dB	>wB	C�B	I�B	K�B	O�B	Q�B	T�B	^5B	aHB	gmB	jB	o�B	x�B	{�B	|�B	}�B	~�B	� B	�B	�B	�1B	�=B	�\B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�?B	�XB	�^B	�dB	�jB	�wB	��B	��B	B	ÖB	ƨB	ƨB	ƨB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�#B	�#B	�)B	�)B	�)B	�/B	�5B	�;B	�BB	�BB	�HB	�HB	�HB	�HB	�HB	�BB	�BB	�BB	�BB	�HB	�TB	�ZB	�ZB	�`B	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
+B
	7B
	7B

=B
DB
DB
DB
DB
DB
DB
DB
JB
JB
JB
JB
PB
VB
VB
\B
\B
bB
hB
hB
hB
hB
oB
oB
oB
oB
oB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
+B
)�B
)�B
+B
+B
+B
+B
,B
-B
-B
.B
.B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
1'B
1'B
2-B
2-B
2-B
2-B
33B
33B
33B
33B
33B
33B
33B
33B
33B
49B
49B
49B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
8RB
8RB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
;dB
<jB
=qB
=qB
=qB
>wB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
=qB
=qB
=qB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
?}B
?}B
?}B
?}B
>wB
>wB
?}B
?}B
?}B
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
B�B
B�B
B�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
F�B
F�B
F�B
G�B
H�B
H�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
I�B
J�B
J�B
J�B
J�B
K�B
J�B
J�B
K�B
L�B
L�B
L�B
L�B
M�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
VB
VB
VB
W
B
W
B
W
B
W
B
XB
XB
XB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
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
`BB
`BB
`BB
`BB
`BB
`BB
`BB
_;B
`BB
aHB
aHB
aHB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
bNB
cTB
cTB
cTB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
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
jB
jB
jB
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
m�B
n�B
n�B
n�B
o�B
o�B
o�B
o�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Bc�BcHBcPBbeBcoBcfBboBcoBbeBbiBcoBciBczBciBciBcoBcrBcrBcrBciBcuBb�Bb�Bc=BfBj�Bb�B^�B^�BXuBD�B1qB&DB �B ^B%B@B�BUBBgB�B�BMB�BsBaB�B�BRB��B�3B�B��B�0B��B��B̅B�+BB��B��B��B&BsHBSBB�B<�B3RB#UBB��B�B�aB��BʶB� B��B�xBk�BG�B1KB)B6BlB
��B
��B
��B
�B
��B
�B
�B
�B
�sB
�PB
�:B
��B
wjB
m�B
aB
K�B
@�B
8oB
/qB
,:B
)[B
%�B
�B
HB
B
ZB
�B
�B
�B	�vB	�B	��B	�LB	�&B	�HB	̼B	�B	�B	�$B	��B	��B	�EB	��B	�}B	�1B	�sB	{dB	wrB	n�B	]�B	X�B	R�B	M4B	I�B	DSB	A`B	?;B	;�B	:�B	3�B	/�B	+�B	)�B	0B	%B	9B	�B	B��B��B�B�B�5B��B��B�B�B�B��B��B�;BׇB�wBԑB��B�}B�;B��B��B�^B��B�RB��B�nB�B��B�4B�eB��B�5B�[B�XB��B�3B�/B�B�kB�B�DB�sB��B�uB�qB}tBy�ByUBw�Bv�Bs�BnBl�Bi�BhSBhQBiBf�BaEB]6B\�B[�BWBV�BVbBT�BS�BPbBOGBNqBL�BI�BEXBB>BAJB@hB?cB>\B=6B=`B=�B=KB<�B=B=/B=~B>B<�B=�B<>B=<BA�BC�BC�BB�BD_BB�BB�BFcBIQBKBK9BLTBQBVhBX9B\GB_tB_�Bb�Bh�Bn+BnQBo�Bs	B��B�ByrBp�Bm�Bl�Bo�Br7Bu1BrEBwjBv�B|�B#B�=B�4B��B��B�;B��B��B��B��B�~B��B��B��B��B�EB�/B�{B�'B��B�B��B��B��B��B��B��B�}B��B�PB�7B;B��B�B��BߔB�/B�B��B�B��B��B��B�TB�B�5B�YB�nB�RB�CB��B�<B�^B�@B	 �B	`B	MB		ZB	�B	�B	�B	�B	�B	�B	�B	KB	�B	cB	�B	�B	�B	�B	B	YB	 B	 B	&JB	&*B	)/B	*B	(�B	*�B	-�B	.�B	/JB	4�B	8�B	;B	<CB	?�B	D{B	JB	LBB	PB	R�B	UcB	^{B	a�B	g�B	j�B	p5B	yB	| B	}B	~-B	LB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�4B	��B	�B	��B	��B	�IB	�)B	�]B	�MB	�B	��B	��B	��B	��B	��B	�"B	�B	�B	��B	�8B	��B	��B	��B	�B	�iB	�bB	�|B	�B	�WB	�mB	�SB	ӌB	�B	�ZB	֞B	��B	�B	�^B	�vB	ړB	�uB	ۊB	�mB	�cB	ܒB	ݶB	��B	��B	�B	�B	�B	��B	�lB	��B	�yB	��B	�B	�B	�B	��B	��B	��B	�B	�B	��B	��B	�<B	��B	��B	��B	��B	��B	�B	�B	�)B	�kB	��B	��B	��B	�B	��B	��B	�B	�B	�B	�B	�B	�-B	�=B	�WB	�5B	�_B	�%B	�&B	�&B	�CB	��B	�JB	�lB	�lB
[B
\B
?B
�B
�B
�B
PB
HB
`B
�B
�B
�B
�B
xB
�B
�B
xB
mB
tB
B
	�B
	�B

�B
{B
�B
�B
B
�B
�B
�B
�B
�B
�B
(B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
'B
B
EB
/B
�B
�B
�B
�B
�B
B
B
B
�B
�B
�B
�B
$B
�B
B
TB
�B
B
2B
�B
AB
B
 B
$B
�B
�B
B
*B
`B
@B
B
�B
B
'B
 OB
!&B
!'B
 �B
!B
!#B
!EB
"OB
#=B
#B
#,B
#{B
$ZB
$-B
$XB
%!B
%$B
%HB
%0B
%DB
%�B
&�B
&ZB
&WB
&TB
'�B
'�B
(qB
(7B
(HB
(�B
(�B
)UB
)�B
)�B
*�B
*LB
+8B
*/B
*�B
+CB
+3B
+cB
+�B
,�B
-~B
-�B
.�B
.�B
/�B
/�B
/�B
0�B
0�B
0�B
0�B
0�B
0�B
1dB
1]B
2tB
2eB
2�B
2�B
2�B
1sB
1iB
2eB
2�B
2jB
2�B
3kB
3_B
3_B
3]B
3]B
3vB
3xB
3�B
3�B
4�B
4�B
4�B
5�B
5�B
5B
5�B
5�B
5�B
6�B
6|B
6�B
6�B
7�B
7�B
7�B
7�B
7�B
7B
7�B
7�B
7�B
7�B
8�B
8�B
8�B
8�B
8�B
8~B
8|B
8�B
9�B
8�B
8�B
9�B
9�B
9�B
9�B
:�B
:�B
:�B
;�B
;�B
;�B
;�B
<B
;�B
<�B
=�B
=�B
=�B
>�B
=�B
=�B
=�B
=�B
=�B
=�B
>�B
=�B
=�B
=�B
>�B
>�B
>�B
?�B
?�B
?�B
?�B
?�B
?�B
?�B
?�B
?�B
?�B
>�B
?9B
?�B
?�B
@B
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
CB
B�B
B�B
DB
D�B
D�B
D�B
D�B
D�B
E�B
E�B
FB
GB
G!B
GB
G�B
H�B
H�B
J	B
JB
KB
J�B
KB
KZB
KFB
JB
K8B
J�B
KB
K'B
L	B
J�B
K4B
L`B
MB
L�B
MB
MB
NAB
OIB
OB
OB
PB
PPB
PDB
PqB
Q0B
Q2B
QB
QNB
Q�B
R+B
RB
SB
S8B
S�B
SVB
S@B
T:B
TOB
TFB
TFB
U3B
UNB
UNB
UkB
UCB
V<B
VrB
VgB
WJB
W4B
WEB
W�B
XRB
X`B
X_B
YzB
Y�B
YtB
ZbB
ZbB
ZYB
ZXB
Z�B
Z�B
[�B
[\B
[ZB
[uB
\oB
\`B
\[B
\tB
\`B
\�B
\^B
\�B
]�B
^jB
^cB
^uB
^�B
^�B
^�B
_�B
_qB
_|B
_zB
_�B
_rB
_qB
`yB
`nB
`�B
`�B
`�B
`�B
`pB
`�B
`�B
`yB
`�B
`�B
`�B
`yB
`mB
`yB
`�B
_�B
`oB
a�B
a�B
a~B
b�B
b�B
b�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
b�B
c�B
c�B
csB
ewB
e�B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
h�B
h�B
i�B
i�B
i�B
i�B
i�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
mB
l�B
l�B
m�B
m�B
m�B
nB
m�B
m�B
m�B
m�B
m�B
m�B
n�B
nB
oB
n�B
oB
o�B
o�B
o�B
o�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<+=�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<5^<#�
<:gA<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<6��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ REPORTED_SURFACE_PRESSURE =0.33 dbar                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CTM alpha = 0.141 & tau = 6.68 s with error equal to the correction                                                                                                                                                                                             Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  201603101134402016031011344020160310113440  AO  ARCAADJP                                                                    20140721233851    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721233851  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721233851  QCF$                G�O�G�O�G�O�0                                                                                                                                   G�O�G�O�G�O�                PM  ARSQPADJV1.1                                                                20160310113440  QC  PRES            @���D�� G�O�                PM  ARSQCTM V1.1                                                                20160310113440  QC  PSAL            @���D�� G�O�                PM  ARSQOWGUV1.0WOD + Argo                                                      20170523133325  IP                  G�O�G�O�G�O�                