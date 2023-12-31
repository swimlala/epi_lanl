CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-06-08T10:01:28Z creation      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p@   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �|   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �L   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �8   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �@   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �H   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220608100128  20220608100128  5906156 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               sA   AO  7912                            2B  A   NAVIS_A                         1020                            170425                          863 @���$L1   @���8��@8�+J�c.I�^51   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         sA   A   A   @@  @y��@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DFfDF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Diy�Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @*�H@dz�@�p�@�p�A�RA:�RAZ�RAz�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B�B�B�B&�B.�B6�B>�BF�BN�BV�B^�Bf�Bn�Bv�B~�B�W
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
��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��D j�D ��D!j�D!��D"j�D"��D#j�D#��D$j�D$��D%j�D%��D&j�D&��D'j�D'��D(j�D(��D)j�D)��D*j�D*��D+j�D+��D,j�D,��D-j�D-��D.j�D.��D/j�D/��D0j�D0��D1j�D1��D2j�D2��D3j�D3��D4j�D4��D5j�D5��D6j�D6��D7j�D7��D8j�D8��D9j�D9��D:j�D:��D;j�D;��D<j�D<��D=j�D=��D>j�D>��D?j�D?��D@j�D@��DAj�DA��DBj�DB��DCj�DC��DDj�DD��DEj�DE�GDFj�DF��DGj�DG��DHj�DH��DIj�DI��DJj�DJ��DKj�DK��DLj�DL��DMj�DM��DNj�DN��DOj�DO��DPj�DP��DQj�DQ��DRj�DR��DSj�DS��DTj�DT��DUj�DU��DVj�DV��DWj�DW��DXj�DX��DYj�DY��DZj�DZ��D[j�D[��D\j�D\��D]j�D]��D^j�D^��D_j�D_��D`j�D`��Daj�Da��Dbj�Db��Dcj�Dc��Ddj�Dd��Dej�De��Dfj�Df��Dgj�Dg��Dhj�Dh��Did{Di��Djj�Dj��Dkj�Dk��Dlj�Dl��Dmj�Dm��Dnj�Dn��Doj�Do��Dpj�Dp��Dqj�Dq��Drj�Dr��Dsj�Ds��Dtj�Dt��Duj�Du��Dvj�Dv��Dwj�Dw��Dxj�Dx��Dyj�Dy��Dzj�Dz��D{j�D{��D|j�D|��D}j�D}��D~j�D~��Dj�D��D�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�2>D�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqDµqD��qD�5qD�uqDõqD��qD�5qD�uqDĵqD��qD�5qD�uqDŵqD��qD�5qD�uqDƵqD��qD�5qD�uqDǵqD��qD�5qD�uqDȵqD��qD�5qD�uqDɵqD��qD�5qD�uqDʵqD��qD�5qD�uqD˵qD��qD�5qD�uqD̵qD��qD�5qD�uqD͵qD��qD�5qD�uqDεqD��qD�5qD�uqDϵqD��qD�5qD�uqDеqD��qD�5qD�uqDѵqD��qD�5qD�uqDҵqD��qD�5qD�uqDӵqD��qD�5qD�uqDԵqD��qD�5qD�uqDյqD��qD�5qD�uqDֵqD��qD�5qD�uqD׵qD��qD�5qD�uqDصqD��qD�5qD�uqDٵqD��qD�5qD�uqDڵqD��qD�5qD�uqD۵qD��qD�5qD�uqDܵqD��qD�5qD�uqDݵqD��qD�5qD�uqD޵qD��qD�5qD�uqDߵqD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�8�D�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD��qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD��qD��qD�5qD�uqD���D��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�8�D�o111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AʬAʸRA�ȴA���Aʺ^A�Aɰ!A�5?A�x�A���A�t�A�%Aĺ^A�jA��A���A�ffA���A���A�{A��A���A�(�A���A�$�A�z�A�n�A��yA� �A�M�A���A�/A��`A�-A�n�A�?}A���A�jA�K�A�-A��A��\A���A�(�A�l�A�9XA�  A��TA��A�5?A�dZA��uA��uA�K�A���A��`A�ƨA���A�x�A�+A�bNA���A�bA���A�VA��;A��7A�&�A���A��A�^5A�5?A���A�bNA�v�A��A�x�A�A���A���A�"�A�?}A�|�A��yA���A��A��A�"�A�C�A��A��^A�`BA��A��A��9A��uA�t�A��PA�hsA��A�\)A�n�A~�A{dZAy�hAxn�Awx�Av�Au�FAtn�ArQ�Al�AhQ�Af�Ae��Ae�Aa��A^�DA]|�A\�AZ1'AX��AV�AUl�AR�`AO�#AMt�AL�\AL{AJ�jAI&�AH�\AF{AD~�AB �A?�hA=��A<jA:��A:^5A:{A8�yA7�A6bNA5p�A4�yA4��A3��A3%A2�DA2E�A1�A1O�A0�`A/�A.��A.�A-33A,�RA,Q�A,$�A+dZA+�A*�A)|�A(JA& �A$�jA$n�A$�A"VA!K�A bNA"�AA��A�/A��Ap�A��A  A��AbA"�AVA�Al�AoA-A\)A��A�hA��A�AA/A�/A��A�A
��A
��A
VA	�A	p�A �A�hA33A��A�AO�A%A�AjA;dA�`A�\A�mA`BA �/A bN@���@�ȴ@�p�@�ƨ@���@���@�z�@���@��\@���@��9@�"�@��D@��@���@��@�D@��@��@��@�I�@��@�%@ڏ\@���@�bN@��@�=q@�%@�(�@�o@Ѻ^@мj@�  @�@��T@̼j@�A�@�ƨ@�"�@�{@Ȭ@�S�@Ɨ�@��T@�?}@ě�@��@�S�@�M�@�x�@���@��@�(�@�  @���@�l�@�n�@��#@�x�@���@���@�o@��+@�x�@�Ĝ@���@�j@���@��@�n�@�J@��7@�%@�Z@��m@�|�@�@�n�@�hs@���@��@���@�bN@�1@�dZ@���@���@�x�@�&�@�V@�j@��
@��@�l�@�ȴ@�ff@�{@��7@�%@���@�r�@�1@�S�@���@��+@�@�?}@���@��@�A�@��w@�dZ@�+@�ȴ@�~�@�=q@�J@���@���@�b@��@��@�l�@�+@�
=@���@�v�@��@�O�@���@�Ĝ@���@��D@�A�@�(�@�  @���@�|�@�C�@�+@�@��H@�ȴ@��!@�^5@���@�G�@���@�z�@�A�@�1@�b@�(�@�9X@��@�ƨ@�ƨ@���@��m@�9X@�1'@�  @���@���@�"�@�@���@���@��/@�r�@�Q�@� �@��;@��m@�S�@��@�O�@�`B@�x�@��@��7@��@�J@�5?@�M�@�E�@�J@��@�O�@�X@�/@��D@�1@�dZ@���@��R@�n�@�n�@�V@�$�@�@��-@�x�@�O�@�7L@�O�@�/@��@��@��D@�bN@�1'@�1@��@���@���@�S�@���@�ff@�E�@�{@��#@�@��-@��7@�p�@�X@�O�@�/@�&�@��@��9@��@��@� �@��;@���@�l�@�C�@�o@��H@��R@���@�v�@�^5@�=q@�-@�{@��-@���@���@��^@�@���@�O�@��@�V@�Ĝ@��@�bN@�j@�bN@�I�@�b@��@��
@��@���@�K�@�
=@���@���@�n�@��@�$�@�V@�5?@��@��@��T@�@�?}@�V@�V@�V@���@��@��@��D@��@�r�@�1'@�@�@~�R@~��@~{@}?}@}V@|��@|��@|�D@|�D@|I�@{�
@{��@{o@z~�@z^5@zM�@y��@y&�@y%@x��@x�`@x��@x�9@xA�@w�;@w��@w\)@vȴ@vE�@u��@up�@u?}@t��@tI�@s��@r�H@r^5@rM�@r�@q��@pĜ@p�@p1'@p  @o�w@o�@o|�@o\)@o;d@n�@nE�@n@m��@m/@l��@l9X@kƨ@k��@k��@kS�@ko@j��@j�!@j�\@j^5@jM�@j=q@jM�@j=q@ihs@h�u@h1'@g�@g\)@f�@fV@f$�@e�-@e��@e��@e��@ep�@d��@d��@d��@d9X@d1@c�
@c��@b�@bn�@b=q@bJ@a�@a�#@a��@a��@aX@`��@`A�@`b@`  @_�@^��@^��@^��@^v�@^v�@^V@^{@\�@\I�@[��@[dZ@[o@Z��@Z��@ZM�@Z=q@Z-@Y�@Y7L@Y%@X�`@XĜ@XA�@W�w@W
=@Vv�@Vff@VE�@V5?@V@U��@U/@Tz�@T(�@S�
@St�@R�\@R�@RJ@Qhs@P��@P��@PĜ@P��@P�u@Pr�@P �@O�P@O
=@N�@N�+@N{@M�T@M��@M`B@L��@L9X@KdZ@KdZ@K"�@K@J��@J�\@JJ@I�@I��@I��@Ihs@IX@IG�@I7L@I&�@H��@Hb@G|�@G�@F�@F�R@F��@E�T@E�h@Ep�@EO�@D�/@DI�@C�
@C��@CdZ@C"�@B��@B�!@B�\@B=q@BJ@BJ@A�#@A�^@A�7@A7L@A�@@�`@@1'@@  @?�@?��@>��@>��@>V@=�T@=V@<��@<��@<��@<�j@<�D@<z�@<j@<9X@<1@;�
@;�@;33@:��@:�\@:�@9�^@9�7@9hs@97L@9%@8�9@81'@7�w@7l�@7K�@7;d@7
=@6ȴ@6��@6ff@5�T@5��@5�@4�@4�j@4��@4Z@4(�@4�@41@3ƨ@3��@3C�@3@2��@2�\@2=q@1��@1��@1�7@17L@0��@0�9@0�u@0A�@/�P@/
=@.�@.��@.��@.ff@.5?@.$�@.{@.@-�T@-�h@-?}@-V@,�@,9X@,(�@,�@+��@+��@+�m@+�m@+t�@+33@+@*�H@*�!@*�!@*�\@*~�@*n�@*�@*J@)��@)�7@)X@)&�@(Ĝ@(�@(A�@(b@(  @'�@'�;@'��@'�w@'�@'|�@'�@&�@&�+@&�+@&ff@&@%�-@%O�@$��@$��@$j@$9X@$(�@$(�@$�@#��@#�F@#�@#@"n�@"-@"�@"�@"J@!�@!��@!��@!X@!X@!G�@!&�@ ��@ ��@ r�@ Q�@ 1'@  �@�@�;@��@;d@ȴ@��@�+@$�@�@��@@�h@p�@?}@��@��@9X@dZ@"�@�@��@�!@��@��@�\@n�@-@��@�#@��@�7@X@7L@%@�`@�u@A�@  @�w@\)@;d@�@�@�+@ff@ff@V@{@�T@�-@�h@`B@?}@��@�@��@��@��@�/@��@�@z�@1@ƨ@dZ@dZ@@�\@�@��@��@�7@hs@X@7L@�@%@�`@�9@bN@A�@�;@��@��@�P@\)@+@�@�R@v�@5?@@�@@�-@��@�@V@�/@��@z�@Z@Z@I�@9X@(�@��@ƨ@ƨ@�F@��@�@dZ@dZ@S�@"�@
�@
��@
�!@
n�@
M�@
J@	��@	x�@	x�@	hs@	G�@	&�@�`@��@��@�9@�@�u111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AʬAʸRA�ȴA���Aʺ^A�Aɰ!A�5?A�x�A���A�t�A�%Aĺ^A�jA��A���A�ffA���A���A�{A��A���A�(�A���A�$�A�z�A�n�A��yA� �A�M�A���A�/A��`A�-A�n�A�?}A���A�jA�K�A�-A��A��\A���A�(�A�l�A�9XA�  A��TA��A�5?A�dZA��uA��uA�K�A���A��`A�ƨA���A�x�A�+A�bNA���A�bA���A�VA��;A��7A�&�A���A��A�^5A�5?A���A�bNA�v�A��A�x�A�A���A���A�"�A�?}A�|�A��yA���A��A��A�"�A�C�A��A��^A�`BA��A��A��9A��uA�t�A��PA�hsA��A�\)A�n�A~�A{dZAy�hAxn�Awx�Av�Au�FAtn�ArQ�Al�AhQ�Af�Ae��Ae�Aa��A^�DA]|�A\�AZ1'AX��AV�AUl�AR�`AO�#AMt�AL�\AL{AJ�jAI&�AH�\AF{AD~�AB �A?�hA=��A<jA:��A:^5A:{A8�yA7�A6bNA5p�A4�yA4��A3��A3%A2�DA2E�A1�A1O�A0�`A/�A.��A.�A-33A,�RA,Q�A,$�A+dZA+�A*�A)|�A(JA& �A$�jA$n�A$�A"VA!K�A bNA"�AA��A�/A��Ap�A��A  A��AbA"�AVA�Al�AoA-A\)A��A�hA��A�AA/A�/A��A�A
��A
��A
VA	�A	p�A �A�hA33A��A�AO�A%A�AjA;dA�`A�\A�mA`BA �/A bN@���@�ȴ@�p�@�ƨ@���@���@�z�@���@��\@���@��9@�"�@��D@��@���@��@�D@��@��@��@�I�@��@�%@ڏ\@���@�bN@��@�=q@�%@�(�@�o@Ѻ^@мj@�  @�@��T@̼j@�A�@�ƨ@�"�@�{@Ȭ@�S�@Ɨ�@��T@�?}@ě�@��@�S�@�M�@�x�@���@��@�(�@�  @���@�l�@�n�@��#@�x�@���@���@�o@��+@�x�@�Ĝ@���@�j@���@��@�n�@�J@��7@�%@�Z@��m@�|�@�@�n�@�hs@���@��@���@�bN@�1@�dZ@���@���@�x�@�&�@�V@�j@��
@��@�l�@�ȴ@�ff@�{@��7@�%@���@�r�@�1@�S�@���@��+@�@�?}@���@��@�A�@��w@�dZ@�+@�ȴ@�~�@�=q@�J@���@���@�b@��@��@�l�@�+@�
=@���@�v�@��@�O�@���@�Ĝ@���@��D@�A�@�(�@�  @���@�|�@�C�@�+@�@��H@�ȴ@��!@�^5@���@�G�@���@�z�@�A�@�1@�b@�(�@�9X@��@�ƨ@�ƨ@���@��m@�9X@�1'@�  @���@���@�"�@�@���@���@��/@�r�@�Q�@� �@��;@��m@�S�@��@�O�@�`B@�x�@��@��7@��@�J@�5?@�M�@�E�@�J@��@�O�@�X@�/@��D@�1@�dZ@���@��R@�n�@�n�@�V@�$�@�@��-@�x�@�O�@�7L@�O�@�/@��@��@��D@�bN@�1'@�1@��@���@���@�S�@���@�ff@�E�@�{@��#@�@��-@��7@�p�@�X@�O�@�/@�&�@��@��9@��@��@� �@��;@���@�l�@�C�@�o@��H@��R@���@�v�@�^5@�=q@�-@�{@��-@���@���@��^@�@���@�O�@��@�V@�Ĝ@��@�bN@�j@�bN@�I�@�b@��@��
@��@���@�K�@�
=@���@���@�n�@��@�$�@�V@�5?@��@��@��T@�@�?}@�V@�V@�V@���@��@��@��D@��@�r�@�1'@�@�@~�R@~��@~{@}?}@}V@|��@|��@|�D@|�D@|I�@{�
@{��@{o@z~�@z^5@zM�@y��@y&�@y%@x��@x�`@x��@x�9@xA�@w�;@w��@w\)@vȴ@vE�@u��@up�@u?}@t��@tI�@s��@r�H@r^5@rM�@r�@q��@pĜ@p�@p1'@p  @o�w@o�@o|�@o\)@o;d@n�@nE�@n@m��@m/@l��@l9X@kƨ@k��@k��@kS�@ko@j��@j�!@j�\@j^5@jM�@j=q@jM�@j=q@ihs@h�u@h1'@g�@g\)@f�@fV@f$�@e�-@e��@e��@e��@ep�@d��@d��@d��@d9X@d1@c�
@c��@b�@bn�@b=q@bJ@a�@a�#@a��@a��@aX@`��@`A�@`b@`  @_�@^��@^��@^��@^v�@^v�@^V@^{@\�@\I�@[��@[dZ@[o@Z��@Z��@ZM�@Z=q@Z-@Y�@Y7L@Y%@X�`@XĜ@XA�@W�w@W
=@Vv�@Vff@VE�@V5?@V@U��@U/@Tz�@T(�@S�
@St�@R�\@R�@RJ@Qhs@P��@P��@PĜ@P��@P�u@Pr�@P �@O�P@O
=@N�@N�+@N{@M�T@M��@M`B@L��@L9X@KdZ@KdZ@K"�@K@J��@J�\@JJ@I�@I��@I��@Ihs@IX@IG�@I7L@I&�@H��@Hb@G|�@G�@F�@F�R@F��@E�T@E�h@Ep�@EO�@D�/@DI�@C�
@C��@CdZ@C"�@B��@B�!@B�\@B=q@BJ@BJ@A�#@A�^@A�7@A7L@A�@@�`@@1'@@  @?�@?��@>��@>��@>V@=�T@=V@<��@<��@<��@<�j@<�D@<z�@<j@<9X@<1@;�
@;�@;33@:��@:�\@:�@9�^@9�7@9hs@97L@9%@8�9@81'@7�w@7l�@7K�@7;d@7
=@6ȴ@6��@6ff@5�T@5��@5�@4�@4�j@4��@4Z@4(�@4�@41@3ƨ@3��@3C�@3@2��@2�\@2=q@1��@1��@1�7@17L@0��@0�9@0�u@0A�@/�P@/
=@.�@.��@.��@.ff@.5?@.$�@.{@.@-�T@-�h@-?}@-V@,�@,9X@,(�@,�@+��@+��@+�m@+�m@+t�@+33@+@*�H@*�!@*�!@*�\@*~�@*n�@*�@*J@)��@)�7@)X@)&�@(Ĝ@(�@(A�@(b@(  @'�@'�;@'��@'�w@'�@'|�@'�@&�@&�+@&�+@&ff@&@%�-@%O�@$��@$��@$j@$9X@$(�@$(�@$�@#��@#�F@#�@#@"n�@"-@"�@"�@"J@!�@!��@!��@!X@!X@!G�@!&�@ ��@ ��@ r�@ Q�@ 1'@  �@�@�;@��@;d@ȴ@��@�+@$�@�@��@@�h@p�@?}@��@��@9X@dZ@"�@�@��@�!@��@��@�\@n�@-@��@�#@��@�7@X@7L@%@�`@�u@A�@  @�w@\)@;d@�@�@�+@ff@ff@V@{@�T@�-@�h@`B@?}@��@�@��@��@��@�/@��@�@z�@1@ƨ@dZ@dZ@@�\@�@��@��@�7@hs@X@7L@�@%@�`@�9@bN@A�@�;@��@��@�P@\)@+@�@�R@v�@5?@@�@@�-@��@�@V@�/@��@z�@Z@Z@I�@9X@(�@��@ƨ@ƨ@�F@��@�@dZ@dZ@S�@"�@
�@
��@
�!@
n�@
M�@
J@	��@	x�@	x�@	hs@	G�@	&�@�`@��@��@�9@�@�u111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B1'B/B.B/B.B-B$�B�B{BuBuBbBhB�B�B{BoB�B�B�B �B�B(�B<jBE�BG�BA�B'�BhBPB�BuBbBB��B�mB�HB�ZB	7B,B+B!�BB��BB�-B��B�XB�FB�LBȴB�#B�TB�B�B�B�B�B�sB�`B�/B��B��BŢB��B�XB�'B��B��B��B�B� Bs�Bm�BXBJ�B=qB�BB�sBƨB�-B��B��B�BZBE�B/B�B\BBB
��B
�B
�}B
�B
��B
�DB
x�B
_;B
P�B
D�B
9XB
�B
uB
DB
B	��B	��B	�B	�#B	�jB	��B	�=B	�B	�B	q�B	[#B	R�B	J�B	?}B	6FB	-B	 �B	�B	B��B�B�B�yB�BB�/B��B��BÖB�dB�-B�B��B��B��B��B��B��B��B��B�{B�{B�bB�hB�VB�VB�DB�DB�B�B�B~�B|�Bz�Bx�Bw�Bt�Br�Bo�Bl�BhsBdZBbNBaHB`BBZBXBW
BR�BP�BN�BL�BK�BJ�BI�BH�BF�BE�BB�BB�B@�B?}B>wB<jB;dB9XB:^B9XB8RB8RB7LB7LB8RB8RB7LB7LB7LB8RB9XB9XB9XB8RB:^B9XB8RB9XB8RB9XB7LB7LB6FB5?B6FB6FB5?B49B49B33B33B2-B2-B2-B33B2-B2-B33B33B1'B0!B/B'�B&�B&�B(�B(�B(�B+B.B.B/B1'B1'B33B33B49B6FB6FB8RB8RB:^B<jB<jB?}BB�BD�BG�BJ�BL�BM�BM�BO�BQ�BR�BW
BXBYBYB[#B[#B\)B_;B`BB`BBaHBcTBe`BgmBiyBn�Bo�Bp�Bp�Br�Bt�Bv�Bv�Bw�Bx�B{�B~�B�B�B�%B�7B�=B�=B�=B�JB�VB�oB��B��B��B��B��B��B��B��B��B�B�B�!B�9B�LB�XB�^B�qB��BĜBƨB��B��B��B�B�
B�B�)B�5B�HB�TB�ZB�ZB�`B�B�B��B��B��B��B��B	B	+B	
=B	DB	VB	{B	�B	�B	�B	�B	�B	�B	�B	!�B	!�B	"�B	%�B	&�B	'�B	(�B	)�B	/B	1'B	2-B	33B	6FB	;dB	>wB	C�B	G�B	I�B	K�B	O�B	P�B	VB	W
B	[#B	[#B	\)B	\)B	\)B	\)B	\)B	_;B	cTB	dZB	e`B	ffB	iyB	o�B	r�B	t�B	v�B	w�B	x�B	z�B	�%B	�7B	�JB	�VB	�\B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�-B	�?B	�LB	�RB	�RB	�XB	�^B	�dB	�jB	�wB	�}B	�}B	��B	��B	B	ÖB	ŢB	ƨB	ȴB	ȴB	ȴB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�)B	�5B	�;B	�BB	�NB	�TB	�TB	�ZB	�`B	�ZB	�ZB	�ZB	�ZB	�`B	�`B	�`B	�`B	�`B	�fB	�mB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
%B
B
%B
%B
+B
	7B
	7B

=B

=B

=B
DB
JB
JB
PB
PB
PB
\B
\B
bB
bB
hB
oB
oB
hB
hB
oB
oB
oB
oB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
!�B
!�B
"�B
"�B
#�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
(�B
(�B
)�B
)�B
)�B
)�B
+B
+B
+B
,B
+B
+B
+B
,B
,B
,B
,B
-B
-B
-B
-B
.B
.B
.B
.B
.B
/B
1'B
1'B
1'B
1'B
1'B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
33B
33B
49B
5?B
5?B
49B
49B
5?B
5?B
5?B
6FB
7LB
7LB
7LB
8RB
7LB
8RB
8RB
9XB
9XB
9XB
:^B
:^B
;dB
;dB
;dB
;dB
<jB
=qB
>wB
?}B
@�B
@�B
A�B
@�B
@�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
D�B
C�B
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
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
J�B
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
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
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
R�B
Q�B
Q�B
Q�B
R�B
S�B
S�B
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
VB
VB
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
YB
YB
YB
YB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
`BB
`BB
`BB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
aHB
bNB
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
e`B
e`B
e`B
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
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
jB
k�B
k�B
k�B
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
n�B
n�B
n�B
n�B
o�B
p�B
p�B
q�B
q�B
q�B
q�B
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
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
w�B
w�B
x�B
x�B
x�B
x�B
y�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
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
{�B
{�B
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
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B1'B/B.B/B.B-B$�B�B{BuBuBbBhB�B�B{BoB�B�B�B �B�B(�B<jBE�BG�BA�B'�BhBPB�BuBbBB��B�mB�HB�ZB	7B,B+B!�BB��BB�-B��B�XB�FB�LBȴB�#B�TB�B�B�B�B�B�sB�`B�/B��B��BŢB��B�XB�'B��B��B��B�B� Bs�Bm�BXBJ�B=qB�BB�sBƨB�-B��B��B�BZBE�B/B�B\BBB
��B
�B
�}B
�B
��B
�DB
x�B
_;B
P�B
D�B
9XB
�B
uB
DB
B	��B	��B	�B	�#B	�jB	��B	�=B	�B	�B	q�B	[#B	R�B	J�B	?}B	6FB	-B	 �B	�B	B��B�B�B�yB�BB�/B��B��BÖB�dB�-B�B��B��B��B��B��B��B��B��B�{B�{B�bB�hB�VB�VB�DB�DB�B�B�B~�B|�Bz�Bx�Bw�Bt�Br�Bo�Bl�BhsBdZBbNBaHB`BBZBXBW
BR�BP�BN�BL�BK�BJ�BI�BH�BF�BE�BB�BB�B@�B?}B>wB<jB;dB9XB:^B9XB8RB8RB7LB7LB8RB8RB7LB7LB7LB8RB9XB9XB9XB8RB:^B9XB8RB9XB8RB9XB7LB7LB6FB5?B6FB6FB5?B49B49B33B33B2-B2-B2-B33B2-B2-B33B33B1'B0!B/B'�B&�B&�B(�B(�B(�B+B.B.B/B1'B1'B33B33B49B6FB6FB8RB8RB:^B<jB<jB?}BB�BD�BG�BJ�BL�BM�BM�BO�BQ�BR�BW
BXBYBYB[#B[#B\)B_;B`BB`BBaHBcTBe`BgmBiyBn�Bo�Bp�Bp�Br�Bt�Bv�Bv�Bw�Bx�B{�B~�B�B�B�%B�7B�=B�=B�=B�JB�VB�oB��B��B��B��B��B��B��B��B��B�B�B�!B�9B�LB�XB�^B�qB��BĜBƨB��B��B��B�B�
B�B�)B�5B�HB�TB�ZB�ZB�`B�B�B��B��B��B��B��B	B	+B	
=B	DB	VB	{B	�B	�B	�B	�B	�B	�B	�B	!�B	!�B	"�B	%�B	&�B	'�B	(�B	)�B	/B	1'B	2-B	33B	6FB	;dB	>wB	C�B	G�B	I�B	K�B	O�B	P�B	VB	W
B	[#B	[#B	\)B	\)B	\)B	\)B	\)B	_;B	cTB	dZB	e`B	ffB	iyB	o�B	r�B	t�B	v�B	w�B	x�B	z�B	�%B	�7B	�JB	�VB	�\B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�-B	�?B	�LB	�RB	�RB	�XB	�^B	�dB	�jB	�wB	�}B	�}B	��B	��B	B	ÖB	ŢB	ƨB	ȴB	ȴB	ȴB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�)B	�5B	�;B	�BB	�NB	�TB	�TB	�ZB	�`B	�ZB	�ZB	�ZB	�ZB	�`B	�`B	�`B	�`B	�`B	�fB	�mB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
%B
B
%B
%B
+B
	7B
	7B

=B

=B

=B
DB
JB
JB
PB
PB
PB
\B
\B
bB
bB
hB
oB
oB
hB
hB
oB
oB
oB
oB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
!�B
!�B
"�B
"�B
#�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
(�B
(�B
)�B
)�B
)�B
)�B
+B
+B
+B
,B
+B
+B
+B
,B
,B
,B
,B
-B
-B
-B
-B
.B
.B
.B
.B
.B
/B
1'B
1'B
1'B
1'B
1'B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
33B
33B
49B
5?B
5?B
49B
49B
5?B
5?B
5?B
6FB
7LB
7LB
7LB
8RB
7LB
8RB
8RB
9XB
9XB
9XB
:^B
:^B
;dB
;dB
;dB
;dB
<jB
=qB
>wB
?}B
@�B
@�B
A�B
@�B
@�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
D�B
C�B
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
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
J�B
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
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
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
R�B
Q�B
Q�B
Q�B
R�B
S�B
S�B
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
VB
VB
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
YB
YB
YB
YB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
`BB
`BB
`BB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
aHB
bNB
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
e`B
e`B
e`B
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
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
jB
k�B
k�B
k�B
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
n�B
n�B
n�B
n�B
o�B
p�B
p�B
q�B
q�B
q�B
q�B
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
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
w�B
w�B
x�B
x�B
x�B
x�B
y�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
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
{�B
{�B
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
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.33 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20220608100128                              AO  ARCAADJP                                                                    20220608100128    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20220608100128  QCP$                G�O�G�O�G�O�205F03E         AO  ARGQQCPL                                                                    20220608100128  QCF$                G�O�G�O�G�O�0               