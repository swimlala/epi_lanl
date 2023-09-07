CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-05-14T18:35:59Z creation      
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
resolution        =���   axis      Z        d  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     d  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     d  _�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     d  oT   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     d  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     d  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     d  �8   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     d  �x   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     d  ˸   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �L   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �L   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �L   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �L   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �x   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �|   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20220514183559  20220514183559  4903032 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  7212                            2B  A   NAVIS_A                         0942                            170425                          863 @���FZN�1   @����-� @<S����d�G�{1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   A   F   @���@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8ffB@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�fC  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�C3D�|�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�=q@�=q@�p�A�RA:�RAZ�RAz�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B�B�B�B&�B.�B7zB>�BF�BN�BV�B^�Bf�Bn�Bv�B~�B�W
B�W
B�W
B�W
B�W
B�W
B�#�B�W
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
Bˊ=B�W
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
C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C��C���C���C���D j�D ��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��D	j�D	��D
j�D
��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��D j�D ��D!j�D!��D"j�D"��D#j�D#��D$j�D$��D%j�D%��D&j�D&��D'j�D'��D(j�D(��D)j�D)��D*j�D*��D+j�D+��D,j�D,��D-j�D-��D.j�D.��D/j�D/��D0j�D0��D1j�D1��D2j�D2��D3j�D3��D4j�D4��D5j�D5��D6j�D6��D7j�D7��D8j�D8��D9j�D9��D:j�D:��D;j�D;��D<j�D<��D=j�D=��D>j�D>��D?j�D?��D@j�D@��DAj�DA��DBj�DB��DCj�DC��DDj�DD��DEj�DE��DFj�DF��DGj�DG��DHj�DH��DIj�DI��DJj�DJ��DKj�DK��DLj�DL��DMj�DM��DNj�DN��DOj�DO��DPj�DP��DQj�DQ��DRj�DR��DSj�DS��DTj�DT��DUj�DU��DVj�DV��DWj�DW��DXj�DX��DYj�DY��DZj�DZ��D[j�D[��D\j�D\��D]j�D]��D^j�D^��D_j�D_��D`j�D`��Daj�Da��Dbj�Db��Dcj�Dc��Ddj�Dd��Dej�De��Dfj�Df��Dgj�Dg��Dhj�Dh��Dij�Di��Djj�Dj��Dkj�Dk��Dlj�Dl��Dmj�Dm��Dnj�Dn��Doj�Do��Dpj�Dp��Dqj�Dq��Drj�Dr��Dsj�Ds��Dtj�Dt��Duj�Du��Dvj�Dv��Dwj�Dw��Dxj�Dx��Dyj�Dy��Dzj�Dz��D{j�D{��D|j�D|��D}j�D}��D~j�D~��Dj�D��D�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqDµqD��qD�5qD�uqDõqD��qD�5qD�uqDĵqD��qD�5qD�uqDŵqD��qD�5qD�uqDƵqD��qD�5qD�uqDǵqD��qD�5qD�uqDȵqD��qD�5qD�uqDɵqD��qD�5qD�uqDʵqD��qD�5qD�uqD˵qD��qD�5qD�uqD̵qD��qD�5qD�uqD͵qD��qD�5qD�uqDεqD��qD�5qD�uqDϵqD��qD�5qD�uqDеqD��qD�5qD�uqDѵqD��qD�5qD�uqDҵqD��qD�5qD�uqDӵqD��qD�5qD�uqDԵqD��qD�5qD�uqDյqD��qD�5qD�uqDֵqD��qD�5qD�uqD׵qD��qD�5qD�uqDصqD��qD�5qD�uqDٵqD��qD�5qD�uqDڵqD��qD�5qD�uqD۵qD��qD�5qD�uqDܵqD��qD�5qD�uqDݵqD��qD�5qD�uqD޵qD��qD�5qD�uqDߵqD��qD�5qD�uqD�qD��qD�5qD�uqD�qD���D�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�>D��>D�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�>D��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD��qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�8�D�r>1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�A��
A�1A���A�ƨA���A��A�t�A�ffA�VA��A��`A�r�A�7LA���A��A��A�K�A�$�A���A��A�`BA�?}A�C�A�hsA��TA�M�A�VA��A�VA��A�p�A���A�5?A���A�/A�ȴA�C�A��A�JA��A���A�ZA��^A�1A�1A���A��;A���A�A�A��yA�Q�A��A��/A��FA��PA�G�A��A���A�VA��A�n�A�JA��9A�9XA��A���A�bNA��A�=qA��A��A��HA�K�A�%A��^A�t�A�
=A�A��hA��mA�%A��A��9A�x�A�\)A�oA�{A�5?A�C�A��jA�?}A��A��A��A���A�z�A�ƨA�JA�A�hsA�bA��
A���A�hsA�&�A���A�l�A���A�Q�A��jA��+A~�A{�wAy�7Ax �Av�At�RAsx�Ar5?Ap��ApbNAol�Am�FAk�;AhffAe�Ae%Adr�Ac��AcdZAcG�Ab��Aa\)A` �A_VA]K�AZffAX�yAW�mAV�+ATM�AS�AS?}AP��AN��AM�ALM�AJȴAJ��AJ�9AJ��AI��AFM�AD�AD  ABĜA@�yA>JA<I�A:�A7t�A6�uA5p�A3�TA2��A2r�A2JA0�yA/��A.r�A,�A*jA(�jA'�;A'��A't�A'33A&~�A$��A#�A"�\A"  A �A��A�A �A�A��A7LAn�A�TAXA��A�AK�A�Ar�A{Ax�A�yA�wA`BA�HA�jAv�AM�AA��A1A�FA�A�
A��A��A�^A|�AVA
ffA	��A	+AZA�hA��Az�A1'A�A�
A�A%AjA�A��AG�AA�9A1'A/@�K�@��R@�@�I�@�dZ@��!@��7@���@�\)@���@�-@��-@��/@�p�@�\)@�^5@��-@�O�@��@�r�@�+@�-@�?}@���@�^5@�x�@�j@�9X@�S�@�
=@ݡ�@�G�@�&�@��@��/@�1'@�+@١�@��@؛�@�1@�
=@�hs@�  @��#@���@��;@�l�@�=q@�O�@̣�@�9X@�(�@���@˶F@˕�@˅@�t�@ˍP@�o@ʟ�@�@�Ĝ@�r�@ǍP@őh@���@�t�@�-@���@��@��`@��y@�J@�z�@�"�@�o@��w@�{@���@�Q�@��@��@��/@��F@��!@��@��@���@�bN@��;@��@���@�bN@���@��T@�bN@�  @�|�@�S�@��H@�=q@��@���@���@�hs@��D@�bN@�b@�S�@�@���@���@��/@�K�@��!@�^5@�O�@���@�I�@�+@��+@�x�@��@�r�@��@�|�@��@�@��+@�@���@�&�@�%@���@��u@�1'@���@�;d@��y@��!@��\@�ff@���@���@�?}@��D@�9X@�1'@��@�1@�  @���@�o@��@��!@�M�@��@�@��h@�`B@�V@��D@�(�@���@��@�t�@�l�@�
=@���@��@�p�@���@�Ĝ@��u@�A�@�(�@���@��w@���@�\)@�;d@�o@��!@�-@��-@�G�@�Ĝ@���@�r�@��
@��F@���@�|�@��@��@���@�~�@��@�{@���@�p�@�G�@�G�@�?}@�/@��`@���@�Q�@�1'@��@�b@�@�w@l�@~�@~��@~V@}�-@|��@|9X@|1@{ƨ@{��@{t�@{@z��@y�@yG�@x��@xr�@w�@w��@w\)@w�@vȴ@vE�@u�@u��@u�h@up�@uO�@u?}@uV@t�@t�j@tZ@sƨ@s"�@r�@r��@rn�@rJ@q��@qhs@qX@qX@q&�@p�u@pbN@pA�@o�;@o��@ol�@o;d@o
=@n�y@n��@n5?@n$�@n$�@n{@m�@mO�@m�@mV@lj@k�m@k��@kS�@k33@j�H@j��@j~�@jM�@i�#@i�^@i7L@h��@h�9@hbN@h �@g�@g|�@g;d@g+@g
=@f�R@fE�@e�@e��@e?}@d9X@c��@c�
@c�F@c�F@c��@c��@ct�@cC�@co@b��@bn�@b-@a�@a��@a��@ahs@a7L@a&�@`��@`�@`bN@`  @_�@_l�@^{@]V@\�@\I�@[��@[�m@[�
@[��@[��@[C�@[@Z^5@Y�^@YG�@Y&�@X�9@XQ�@XA�@X �@W�@W�;@W�@W\)@WK�@W
=@V��@VV@VE�@VE�@V$�@V@V{@V{@U�@U/@U�@T�@T��@T��@T�@Tz�@TZ@T�@S�m@S�F@SS�@R�!@R�@Q��@Q��@Q��@Q�^@Q��@Q�^@Q��@Qhs@Q&�@P��@P�9@PQ�@PA�@P  @O�;@O�w@O�@O\)@Nȴ@N��@Nff@N$�@M��@M��@Mp�@L��@L�@L�D@Lj@L9X@L�@L1@K�m@K�
@Kƨ@K�F@K��@K��@K�@Kt�@KC�@JM�@I�@I��@I&�@H�`@H�9@HbN@HQ�@H1'@Hb@H  @G�;@G��@G��@G\)@F�R@E��@E��@E�@E`B@E/@EV@D�@D��@D�j@D��@D��@D�D@DZ@C�
@C��@CC�@B�@B�!@B^5@A�#@A�7@Ax�@Ahs@AG�@A%@@��@@��@@�@@bN@@Q�@@1'@@ �@@ �@?��@>�R@=�@=p�@<�/@<�@<�@:�@:�\@:~�@:n�@:^5@:M�@:-@9��@9G�@8��@8�@8 �@7�P@7;d@7
=@6�y@6ȴ@6��@6�+@6V@5�@5��@5�@5`B@5/@4��@4�@4j@4(�@3�
@3��@3t�@3C�@2��@2J@1�#@1��@1X@0�`@0r�@0bN@01'@/�;@/l�@/\)@/K�@/;d@.�R@.v�@.$�@-@-�h@-/@-V@,�@,I�@+��@+ƨ@+"�@*n�@)�@)�7@)x�@)x�@)X@)hs@)X@)X@)G�@)7L@)%@(�`@(�9@(�@(r�@(Q�@(1'@'|�@'|�@'l�@'l�@';d@'
=@&�@&�R@&�R@&��@&��@&��@&��@&��@&��@&��@&5?@&{@%�T@%@%�@%�@$��@$��@$�@$�@$��@$j@$Z@$9X@$1@#�F@#C�@"�H@"��@"�\@"�\@"~�@"�@!��@!��@!�^@!7L@ Ĝ@ �@   @��@|�@��@��@v�@�T@�h@�j@��@Z@I�@Z@I�@I�@9X@9X@9X@9X@�@��@�
@��@�@dZ@C�@��@~�@^5@=q@��@��@hs@%@�u@A�@  @�@�w@�P@|�@;d@
=@�y@�y@�@��@v�@E�@V@E�@5?@@�@��@��@p�@?}@?}@O�@O�@O�@O�@`B@`B@p�@�@�h@`B@��@�/@�j@�@�@�D@z�@I�@1@��@33@@��@��@n�@-@�@�^@hs@&�@�9@Q�@Q�@1'@ �@�@�w@��@l�@+@
=@�@�R@�+@V@$�@�@�@{@@�@��@�-@�@O�@/@V@�@�j@j@Z@9X@�@�@�
@ƨ@��@�@�@t�@dZ@C�@33@33@33@"�@o@o@o@o@
�H@
��@
�!@
�\@
~�@
M�@
-@
J@	��@	X@	�@��@��@�u1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�A��
A�1A���A�ƨA���A��A�t�A�ffA�VA��A��`A�r�A�7LA���A��A��A�K�A�$�A���A��A�`BA�?}A�C�A�hsA��TA�M�A�VA��A�VA��A�p�A���A�5?A���A�/A�ȴA�C�A��A�JA��A���A�ZA��^A�1A�1A���A��;A���A�A�A��yA�Q�A��A��/A��FA��PA�G�A��A���A�VA��A�n�A�JA��9A�9XA��A���A�bNA��A�=qA��A��A��HA�K�A�%A��^A�t�A�
=A�A��hA��mA�%A��A��9A�x�A�\)A�oA�{A�5?A�C�A��jA�?}A��A��A��A���A�z�A�ƨA�JA�A�hsA�bA��
A���A�hsA�&�A���A�l�A���A�Q�A��jA��+A~�A{�wAy�7Ax �Av�At�RAsx�Ar5?Ap��ApbNAol�Am�FAk�;AhffAe�Ae%Adr�Ac��AcdZAcG�Ab��Aa\)A` �A_VA]K�AZffAX�yAW�mAV�+ATM�AS�AS?}AP��AN��AM�ALM�AJȴAJ��AJ�9AJ��AI��AFM�AD�AD  ABĜA@�yA>JA<I�A:�A7t�A6�uA5p�A3�TA2��A2r�A2JA0�yA/��A.r�A,�A*jA(�jA'�;A'��A't�A'33A&~�A$��A#�A"�\A"  A �A��A�A �A�A��A7LAn�A�TAXA��A�AK�A�Ar�A{Ax�A�yA�wA`BA�HA�jAv�AM�AA��A1A�FA�A�
A��A��A�^A|�AVA
ffA	��A	+AZA�hA��Az�A1'A�A�
A�A%AjA�A��AG�AA�9A1'A/@�K�@��R@�@�I�@�dZ@��!@��7@���@�\)@���@�-@��-@��/@�p�@�\)@�^5@��-@�O�@��@�r�@�+@�-@�?}@���@�^5@�x�@�j@�9X@�S�@�
=@ݡ�@�G�@�&�@��@��/@�1'@�+@١�@��@؛�@�1@�
=@�hs@�  @��#@���@��;@�l�@�=q@�O�@̣�@�9X@�(�@���@˶F@˕�@˅@�t�@ˍP@�o@ʟ�@�@�Ĝ@�r�@ǍP@őh@���@�t�@�-@���@��@��`@��y@�J@�z�@�"�@�o@��w@�{@���@�Q�@��@��@��/@��F@��!@��@��@���@�bN@��;@��@���@�bN@���@��T@�bN@�  @�|�@�S�@��H@�=q@��@���@���@�hs@��D@�bN@�b@�S�@�@���@���@��/@�K�@��!@�^5@�O�@���@�I�@�+@��+@�x�@��@�r�@��@�|�@��@�@��+@�@���@�&�@�%@���@��u@�1'@���@�;d@��y@��!@��\@�ff@���@���@�?}@��D@�9X@�1'@��@�1@�  @���@�o@��@��!@�M�@��@�@��h@�`B@�V@��D@�(�@���@��@�t�@�l�@�
=@���@��@�p�@���@�Ĝ@��u@�A�@�(�@���@��w@���@�\)@�;d@�o@��!@�-@��-@�G�@�Ĝ@���@�r�@��
@��F@���@�|�@��@��@���@�~�@��@�{@���@�p�@�G�@�G�@�?}@�/@��`@���@�Q�@�1'@��@�b@�@�w@l�@~�@~��@~V@}�-@|��@|9X@|1@{ƨ@{��@{t�@{@z��@y�@yG�@x��@xr�@w�@w��@w\)@w�@vȴ@vE�@u�@u��@u�h@up�@uO�@u?}@uV@t�@t�j@tZ@sƨ@s"�@r�@r��@rn�@rJ@q��@qhs@qX@qX@q&�@p�u@pbN@pA�@o�;@o��@ol�@o;d@o
=@n�y@n��@n5?@n$�@n$�@n{@m�@mO�@m�@mV@lj@k�m@k��@kS�@k33@j�H@j��@j~�@jM�@i�#@i�^@i7L@h��@h�9@hbN@h �@g�@g|�@g;d@g+@g
=@f�R@fE�@e�@e��@e?}@d9X@c��@c�
@c�F@c�F@c��@c��@ct�@cC�@co@b��@bn�@b-@a�@a��@a��@ahs@a7L@a&�@`��@`�@`bN@`  @_�@_l�@^{@]V@\�@\I�@[��@[�m@[�
@[��@[��@[C�@[@Z^5@Y�^@YG�@Y&�@X�9@XQ�@XA�@X �@W�@W�;@W�@W\)@WK�@W
=@V��@VV@VE�@VE�@V$�@V@V{@V{@U�@U/@U�@T�@T��@T��@T�@Tz�@TZ@T�@S�m@S�F@SS�@R�!@R�@Q��@Q��@Q��@Q�^@Q��@Q�^@Q��@Qhs@Q&�@P��@P�9@PQ�@PA�@P  @O�;@O�w@O�@O\)@Nȴ@N��@Nff@N$�@M��@M��@Mp�@L��@L�@L�D@Lj@L9X@L�@L1@K�m@K�
@Kƨ@K�F@K��@K��@K�@Kt�@KC�@JM�@I�@I��@I&�@H�`@H�9@HbN@HQ�@H1'@Hb@H  @G�;@G��@G��@G\)@F�R@E��@E��@E�@E`B@E/@EV@D�@D��@D�j@D��@D��@D�D@DZ@C�
@C��@CC�@B�@B�!@B^5@A�#@A�7@Ax�@Ahs@AG�@A%@@��@@��@@�@@bN@@Q�@@1'@@ �@@ �@?��@>�R@=�@=p�@<�/@<�@<�@:�@:�\@:~�@:n�@:^5@:M�@:-@9��@9G�@8��@8�@8 �@7�P@7;d@7
=@6�y@6ȴ@6��@6�+@6V@5�@5��@5�@5`B@5/@4��@4�@4j@4(�@3�
@3��@3t�@3C�@2��@2J@1�#@1��@1X@0�`@0r�@0bN@01'@/�;@/l�@/\)@/K�@/;d@.�R@.v�@.$�@-@-�h@-/@-V@,�@,I�@+��@+ƨ@+"�@*n�@)�@)�7@)x�@)x�@)X@)hs@)X@)X@)G�@)7L@)%@(�`@(�9@(�@(r�@(Q�@(1'@'|�@'|�@'l�@'l�@';d@'
=@&�@&�R@&�R@&��@&��@&��@&��@&��@&��@&��@&5?@&{@%�T@%@%�@%�@$��@$��@$�@$�@$��@$j@$Z@$9X@$1@#�F@#C�@"�H@"��@"�\@"�\@"~�@"�@!��@!��@!�^@!7L@ Ĝ@ �@   @��@|�@��@��@v�@�T@�h@�j@��@Z@I�@Z@I�@I�@9X@9X@9X@9X@�@��@�
@��@�@dZ@C�@��@~�@^5@=q@��@��@hs@%@�u@A�@  @�@�w@�P@|�@;d@
=@�y@�y@�@��@v�@E�@V@E�@5?@@�@��@��@p�@?}@?}@O�@O�@O�@O�@`B@`B@p�@�@�h@`B@��@�/@�j@�@�@�D@z�@I�@1@��@33@@��@��@n�@-@�@�^@hs@&�@�9@Q�@Q�@1'@ �@�@�w@��@l�@+@
=@�@�R@�+@V@$�@�@�@{@@�@��@�-@�@O�@/@V@�@�j@j@Z@9X@�@�@�
@ƨ@��@�@�@t�@dZ@C�@33@33@33@"�@o@o@o@o@
�H@
��@
�!@
�\@
~�@
M�@
-@
J@	��@	X@	�@��@��@�u1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B��B��B�B�B�!B�?B�RB�XB�XB�LB�?B�3B�B�B�B��B��B��B��B�{B�\B�DB�1B�B}�Bv�Bq�Bn�Bl�Bk�BjBiyBiyBjBiyBjBiyBhsBjBiyBe`B]/BQ�BE�B?}B;dB7LB&�B"�B�B�B�B�BhB%B�B��B�LB�B�{B�JBx�BgmBS�BM�BK�BK�BI�BB�B2-B(�B"�B�BVB%B��B�B�B�B�mB�`B�BB��BB�!B��B�hB|�Bm�BffB[#BJ�B=qB5?B&�B�B�B�BhBVB
=BB��B��B�B�TB�BĜB�9B��B��B�+B~�B{�Bm�BcTB^5BYBN�BE�B9XB.B'�B#�B �B�B�B�B�BVB+B  B�B�sB�HB�/B��B��B��B�}B�FB�B��B��B��B��B��B��B�bB�=B�B� By�Bm�BdZBaHBQ�BK�BH�BB�B?}B<jB9XB6FB.B+B&�B%�B$�B"�B!�B!�B!�B �B�B�B�B�B�B�B�B�B�B{BuBoBhB\BVBPBJBJBDB
=B	7B+B%B%BBBBBBBB  B
��B
��B
��B
��B
��B
��B
��B
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
�B
�yB
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
�B
�B
�B
�B
�B
�B
�B
�B
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
��BBB%B1B1B+B	7B1B%BBBB  BBB  B
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
��B  B
��B  BBBBBB
=B\B{B�B�B�B�B�B �B"�B#�B#�B#�B"�B$�B&�B(�B.B33B49B6FB9XBD�BI�BJ�BO�BR�BXB_;BcTBiyBl�Bp�Bq�Bt�Bv�Bv�Bx�B{�B� B�B�B�%B�+B�1B�7B�VB�bB�oB�uB�{B��B��B��B��B��B��B��B��B��B��B�B�!B�'B�?B�RB�^B�dB�qB��BĜBȴB��B��B��B��B��B��B�B�;B�TB�`B�mB�B�B�B�B�B�B�B��B��B��B��BB%B
=BPBVBVBbBoB{B�B�B�B�B�B�B#�B&�B&�B'�B(�B+B.B0!B1'B1'B1'B1'B2-B2-B5?B6FB7LB9XB<jB@�B@�BB�BB�BC�BE�BF�BJ�BM�BO�BQ�BS�BVBVBW
BYB[#B\)B]/B^5B_;B_;B`BBaHBaHBbNBdZBgmBjBk�Bk�Bm�Bo�Bp�Bq�Br�Br�Br�Bu�Bv�Bv�Bx�By�Bz�B{�B|�B|�B}�B� B� B� B� B�B�B�B�B�1B�DB�JB�PB�VB�\B�bB�hB�oB�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�!B�!B�'B�-B�3B�9B�?B�FB�FB�LB�RB�RB�XB�^B�dB�jB�qB�qBBƨBǮBȴBɺBɺBɺB��B��B��B��B��B��B��B��B��B�B�
B�
B�B�B�B�B�B�#B�)B�/B�/B�/B�5B�5B�5B�5B�BB�HB�HB�HB�NB�NB�TB�TB�ZB�ZB�`B�`B�fB�sB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  BBBBBBBBBBBBB%B1B1B1B	7B	7B	7B
=B
=B
=B
=B
=B
=BDBJBJBPBPBVBVBbBbBbBhBhBhBoBoBuBuBuBuB{BuB{B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B!�B"�B"�B#�B$�B$�B%�B%�B%�B%�B&�B'�B'�B'�B(�B(�B(�B)�B)�B+B+B+B,B,B-B.B/B/B/B0!B1'B1'B1'B2-B33B33B33B33B49B49B5?B6FB6FB7LB7LB7LB8RB9XB9XB:^B;dB<jB=qB=qB=qB=qB=qB=qB=qB>wB>wB>wB?}B?}B?}B?}B?}B@�BA�BA�BA�BA�BA�BB�BB�BB�BC�BC�BC�BC�BC�BC�BC�BC�BD�BD�BD�BD�BE�BE�BF�BF�BF�BF�BF�BG�BG�BG�BH�BH�BI�BI�BJ�BJ�BJ�BJ�BK�BK�BK�BK�BL�BM�BM�BN�BN�BN�BO�BP�BP�BQ�BQ�BS�BS�BS�BS�BS�BS�BT�BT�BT�BT�BT�BT�BT�BVBVBVBVBVBW
BXBXBXBYBYBYBZBZB[#B[#B[#B\)B\)B\)B]/B]/B]/B]/B]/B^5B^5B^5B^5B^5B_;B_;B_;B_;B_;B`BB`BB`BB`BB`BB`BB`BB`BB`BB`BB`BB`BBaHBaHBaHBbNBbNBbNBbNBbNBbNBcTBcTBdZBdZBe`Be`Be`BffBffBffBgmBgmBhsBhsBhsBiyBiyBiyBjBjBjBjBk�Bk�Bk�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bm�Bm�Bm�Bm�Bn�Bn�Bn�Bn�Bo�Bo�Bo�Bp�Bp�Bp�Bp�Bp�Bq�Bq�Bq�Bq�Bq�Br�Br�Br�Br�Br�Br�Br�Br�Br�Br�Bs�Bs�Bs�Bs�Bt�Bt�Bt�Bt�Bu�Bv�Bv�Bv�Bw�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444   B�B�B�B�B�B��B��B�B�B�!B�?B�RB�XB�XB�LB�?B�3B�B�B�B��B��B��B��B�{B�\B�DB�1B�B}�Bv�Bq�Bn�Bl�Bk�BjBiyBiyBjBiyBjBiyBhsBjBiyBe`B]/BQ�BE�B?}B;dB7LB&�B"�B�B�B�B�BhB%B�B��B�LB�B�{B�JBx�BgmBS�BM�BK�BK�BI�BB�B2-B(�B"�B�BVB%B��B�B�B�B�mB�`B�BB��BB�!B��B�hB|�Bm�BffB[#BJ�B=qB5?B&�B�B�B�BhBVB
=BB��B��B�B�TB�BĜB�9B��B��B�+B~�B{�Bm�BcTB^5BYBN�BE�B9XB.B'�B#�B �B�B�B�B�BVB+B  B�B�sB�HB�/B��B��B��B�}B�FB�B��B��B��B��B��B��B�bB�=B�B� By�Bm�BdZBaHBQ�BK�BH�BB�B?}B<jB9XB6FB.B+B&�B%�B$�B"�B!�B!�B!�B �B�B�B�B�B�B�B�B�B�B{BuBoBhB\BVBPBJBJBDB
=B	7B+B%B%BBBBBBBB  B
��B
��B
��B
��B
��B
��B
��B
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
�B
�yB
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
�B
�B
�B
�B
�B
�B
�B
�B
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
��BBB%B1B1B+B	7B1B%BBBB  BBB  B
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
��B  B
��B  BBBBBB
=B\B{B�B�B�B�B�B �B"�B#�B#�B#�B"�B$�B&�B(�B.B33B49B6FB9XBD�BI�BJ�BO�BR�BXB_;BcTBiyBl�Bp�Bq�Bt�Bv�Bv�Bx�B{�B� B�B�B�%B�+B�1B�7B�VB�bB�oB�uB�{B��B��B��B��B��B��B��B��B��B��B�B�!B�'B�?B�RB�^B�dB�qB��BĜBȴB��B��B��B��B��B��B�B�;B�TB�`B�mB�B�B�B�B�B�B�B��B��B��B��BB%B
=BPBVBVBbBoB{B�B�B�B�B�B�B#�B&�B&�B'�B(�B+B.B0!B1'B1'B1'B1'B2-B2-B5?B6FB7LB9XB<jB@�B@�BB�BB�BC�BE�BF�BJ�BM�BO�BQ�BS�BVBVBW
BYB[#B\)B]/B^5B_;B_;B`BBaHBaHBbNBdZBgmBjBk�Bk�Bm�Bo�Bp�Bq�Br�Br�Br�Bu�Bv�Bv�Bx�By�Bz�B{�B|�B|�B}�B� B� B� B� B�B�B�B�B�1B�DB�JB�PB�VB�\B�bB�hB�oB�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�!B�!B�'B�-B�3B�9B�?B�FB�FB�LB�RB�RB�XB�^B�dB�jB�qB�qBBƨBǮBȴBɺBɺBɺB��B��B��B��B��B��B��B��B��B�B�
B�
B�B�B�B�B�B�#B�)B�/B�/B�/B�5B�5B�5B�5B�BB�HB�HB�HB�NB�NB�TB�TB�ZB�ZB�`B�`B�fB�sB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  BBBBBBBBBBBBB%B1B1B1B	7B	7B	7B
=B
=B
=B
=B
=B
=BDBJBJBPBPBVBVBbBbBbBhBhBhBoBoBuBuBuBuB{BuB{B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B!�B"�B"�B#�B$�B$�B%�B%�B%�B%�B&�B'�B'�B'�B(�B(�B(�B)�B)�B+B+B+B,B,B-B.B/B/B/B0!B1'B1'B1'B2-B33B33B33B33B49B49B5?B6FB6FB7LB7LB7LB8RB9XB9XB:^B;dB<jB=qB=qB=qB=qB=qB=qB=qB>wB>wB>wB?}B?}B?}B?}B?}B@�BA�BA�BA�BA�BA�BB�BB�BB�BC�BC�BC�BC�BC�BC�BC�BC�BD�BD�BD�BD�BE�BE�BF�BF�BF�BF�BF�BG�BG�BG�BH�BH�BI�BI�BJ�BJ�BJ�BJ�BK�BK�BK�BK�BL�BM�BM�BN�BN�BN�BO�BP�BP�BQ�BQ�BS�BS�BS�BS�BS�BS�BT�BT�BT�BT�BT�BT�BT�BVBVBVBVBVBW
BXBXBXBYBYBYBZBZB[#B[#B[#B\)B\)B\)B]/B]/B]/B]/B]/B^5B^5B^5B^5B^5B_;B_;B_;B_;B_;B`BB`BB`BB`BB`BB`BB`BB`BB`BB`BB`BB`BBaHBaHBaHBbNBbNBbNBbNBbNBbNBcTBcTBdZBdZBe`Be`Be`BffBffBffBgmBgmBhsBhsBhsBiyBiyBiyBjBjBjBjBk�Bk�Bk�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bm�Bm�Bm�Bm�Bn�Bn�Bn�Bn�Bo�Bo�Bo�Bp�Bp�Bp�Bp�Bp�Bq�Bq�Bq�Bq�Bq�Br�Br�Br�Br�Br�Br�Br�Br�Br�Br�Bs�Bs�Bs�Bs�Bt�Bt�Bt�Bt�Bu�Bv�Bv�Bv�Bw�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.33 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20220514183559                              AO  ARCAADJP                                                                    20220514183559    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20220514183559  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20220514183559  QCF$                G�O�G�O�G�O�8000            