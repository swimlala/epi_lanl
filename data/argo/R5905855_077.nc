CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:24:07Z creation;2022-06-04T19:24:07Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ID   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M8   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �l   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �`   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �$   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �X   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20220604192407  20220610161505  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               MA   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�f��M^p1   @�f�.z�@-X�t�j�c��1'1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   AA��Aa��A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�33B���B���B���B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B���B�  C   C  C  C  C  C
  C�C  C  C  C  C�C��C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD�CF  CG�fCJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǃ3D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ DԼ�D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�&f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @z�@j�H@�p�@�p�A�RA<Q�A\Q�Az�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B�B�B�B&�B.�B6�B>�BF�BN�BV�B^�Bf�Bn�Bv�B~�B�W
B�W
B�W
B�W
B�W
B��=B��B�#�B��B�W
B�W
B�W
B�W
B��B��B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�=B�W
B�W
B�W
B�#�B�W
B�W
C��C��C��C��C	��C�C��C��C��C��C�CxRC��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC�CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C��C���C���C���C���C���C��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D j�D ��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��D	j�D	��D
j�D
��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D�GDj�D��D j�D ��D!j�D!��D"j�D"��D#j�D#��D$j�D$��D%j�D%��D&j�D&��D'j�D'��D(j�D(��D)j�D)��D*j�D*��D+j�D+��D,j�D,��D-j�D-��D.j�D.��D/j�D/��D0j�D0��D1j�D1��D2j�D2��D3j�D3��D4j�D4��D5j�D5��D6j�D6��D7j�D7��D8j�D8��D9j�D9��D:j�D:��D;j�D;��D<j�D<��D=j�D=��D>j�D>��D?j�D?��D@j�D@��DAj�DA��DBj�DB��DCj�DC��DDj�DD��DEj�DE��DFj�DF��DGj�DG��DHj�DH��DIj�DI��DJj�DJ��DKj�DK��DLj�DL��DMj�DM��DNj�DN��DOj�DO��DPj�DP��DQj�DQ��DRj�DR��DSj�DS��DTj�DT��DUj�DU��DVj�DV��DWj�DW��DXj�DX��DYj�DY��DZj�DZ��D[j�D[��D\j�D\��D]j�D]��D^j�D^��D_j�D_��D`j�D`��Daj�Da��Dbj�Db��Dcj�Dc��Ddj�Dd��Dej�De��Dfj�Df��Dgj�Dg��Dhj�Dh��Dij�Di��Djj�Dj��Dkj�Dk��Dlj�Dl��Dmj�Dm��Dnj�Dn��Doj�Do��Dpj�Dp��Dqj�Dq��Drj�Dr��Dsj�Ds��Dtj�Dt��Duj�Du��Dvj�Dv��Dwj�Dw��Dxj�Dx��Dyj�Dy��Dzj�Dz��D{j�D{��D|j�D|��D}j�D}��D~j�D~��Dj�D��D�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqDµqD��qD�5qD�uqDõqD��qD�5qD�uqDĵqD��qD�5qD�uqDŵqD��qD�5qD�uqDƵqD��qD�5qD�x�DǵqD��qD�5qD�uqDȵqD��qD�5qD�uqDɵqD��qD�5qD�uqDʵqD��qD�5qD�uqD˵qD��qD�5qD�uqD̵qD��qD�5qD�uqD͵qD��qD�5qD�uqDεqD��qD�5qD�uqDϵqD��qD�5qD�uqDеqD��qD�5qD�uqDѵqD��qD�5qD�uqDҵqD��qD�5qD�uqDӵqD��qD�5qD�uqDԲ>D��qD�5qD�uqDյqD��qD�5qD�uqDֵqD��qD�5qD�uqD׵qD��qD�5qD�uqDصqD��qD�5qD�uqDٵqD��qD�5qD�uqDڵqD��qD�5qD�uqD۵qD��qD�5qD�uqDܵqD��qD�5qD�uqDݵqD��qD�5qD�uqD޵qD��qD�5qD�uqDߵqD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD��qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��A��A�hA��A��A��A�A���A���A��A��+A��>A��rA��A��A�VA�\A�@A��A��A�!�A�&�A�'�A�&�A�*�A�2aA�49A�9$A�G�A�U�A�:*A��A��|AʣA�AǨ�A�b�A�-�AÿHAÌA�4nA���A��&A��A��A���A��hA���A��rA�xA�!�A�҉A��A�"�A�"�A�J�A��vA�cTA���A��&A��A���A�`vA��oA�(XA�3�A��A�1[A���A� 4A��A��A�E9A��gAƨAy�At��Aq	�Ak�6Af��Adu%AasA_�A\n�AZB�AWqAU��AU$tAS�VARB[AP��AN�AAK�0AI�_AGv�AFbNAE�KAD7LAA�nAA0UA@�A?�A=H�A=��A=�:A=9�A<��A:��A8	lA7�A6T�A0(�A.��A.�jA.ϫA.��A.;dA-�YA,��A,4�A+�A*H�A(��A(?A'�A'dZA&�A%�"A$�;A"VA!�A �!A!MA!��A"xlA"�yA"��A"�A!��A!J#A ��A ��A ƨA ��A ��A *�A zA :�AzxA<6A�A�NAzA��Ae�A�UAa|A�A�AiDA�8A��A^5A~A��AkQA�IAxA��A�YA��AA�A�QAeA��Ao�A1�A�A-A��AA�
AݘA�.A`�A?�A3�AGA�vA�wAZA+kA+A��A*�AƨA�"A�A�,A��A�4A*�A
=A��A�A�hAc A�WA��A�$AW?A
�<A
�nA
�!A
�A
�A
o�A
�A	�)A	�@A	s�A	;A�}A�AMA��A��Al�A��AU2A�A� A|�Al�A�AG�A
=AԕA�3A�[Ax�AMA��AxlA�)A �x@�0�@���@���@�^�@��@�%�@�;d@���@�u�@��;@�;@���@���@���@�K^@�_@�#�@��@���@��'@�>B@�f�@��@��@���@�bN@���@�(@꩓@ꅈ@�:*@���@�l�@�@�.@�&�@�v�@�3�@��z@�h
@�,�@��@��@�xl@�S�@��9@߂�@�5�@���@�D�@���@�%@�ff@���@�u�@���@�GE@�$t@أ@ש*@�7L@�֡@��.@Ԣ4@���@�&�@Ҟ�@��@ѩ�@�;d@�ں@�tT@ϐ�@��@���@Έ�@��K@�=@���@�R�@ˊ�@��@ʦL@�j@�*�@��#@�j�@�@Ȗ�@�@ǖS@�+�@�֡@�C�@��@Ű�@�4@Ğ@�)�@���@Ñh@�W?@��@�@��m@�+@���@�-@���@�6z@��9@�O@�Mj@��@�ی@�J�@��F@�8�@��@���@��I@��o@�Q�@�/�@��@�l�@�@��?@��L@�_�@�e@�u@���@�c�@���@���@��D@�~(@��9@���@�H@�~@��@���@�/�@��@���@�\�@�O@��V@�7L@��|@���@�]d@��N@��n@���@�� @�q�@�?@�@��C@��S@��~@�y�@�6z@��@�r�@�%�@��@��z@�o@��@���@�oi@�Q�@�6�@��@�ߤ@�W�@�ݘ@�zx@�C@���@���@�g8@���@�}�@�]�@�S�@��@��@�}V@�8�@��@��:@�U�@�.I@��@���@���@�5?@��@���@�a�@�Z�@�4�@�q@�
=@���@���@�@���@�P�@���@���@�[�@�V�@�E�@�.�@�x@��@���@���@�<6@��@�@���@���@���@���@��7@�o@���@�4@��#@�s�@�T�@�C�@��@���@���@�A�@��0@��7@��@��4@�Y�@��R@�Xy@�1�@��@�s@��	@��@�r�@�W�@�=q@��@��z@���@��"@��4@�{J@�%F@��@�YK@�.�@��@��N@���@�|@���@�z@��@��Z@���@���@��"@�dZ@�K�@�7L@��@���@���@�Z@��@��@���@�.I@��4@�W�@�7@���@���@�1�@�!�@��,@��j@��r@�M�@�=q@�*�@���@��@�k�@�H�@���@��@�B[@�  @���@�*0@��@��)@�^5@�$�@��3@��:@�j�@��@��@��B@��'@�:�@��t@��"@�e�@�S�@�Mj@�Dg@�:�@�.I@��v@�Ĝ@��@��r@�N�@��r@���@�m]@�X@�2a@��f@��X@���@�Q�@�/�@� @e�@8@�@~�\@~	@}�X@}o @}q@|��@|Q�@{ƨ@{_p@{6z@{�@z��@z�A@z;�@y��@y:�@x�[@xtT@w/�@vL0@v+k@v_@u�H@uVm@t�@t��@tD�@tb@s��@s�@sC�@ra|@r)�@r	@r	@q��@q�z@q��@p�v@o�0@n�8@nV@mϫ@m�=@mF@l�u@lN�@l1@k33@j�X@j��@j�b@j��@j1�@i��@i�h@h�@hZ@g��@gg�@f�8@fa|@e��@eVm@d�[@dD�@d�@c�g@c~�@c1�@b��@b��@b��@b^5@b.�@a��@a�t@a��@azx@aX@a8�@`�E@_U�@^��@^��@^J@]k�@\��@\�@\��@\��@\z�@\c�@\/�@[��@[�@[K�@[Mj@[A�@Z�"@Z�6@ZE�@Y��@Y5�@X��@X�Y@Wƨ@V��@V��@V�A@Vh
@VJ�@V)�@U��@U�N@U�@Uc�@U?}@U	l@T�O@TZ@T'R@S�@SX�@S6z@S�@RZ�@RJ@P��@PXy@P9X@O��@O�@NQ@M�T@M<6@L�f@Loi@K��@K�F@K~�@KS�@K!-@J�<@Jxl@I�@Ic@IB�@H�	@H�@HɆ@H��@H	�@Gg�@F��@F��@Fi�@F	@E�^@Em]@E8�@E�@D�I@D"h@C�@C�$@B�M@B��@B��@Ba|@B)�@B
�@A��@A�@Au�@AA @A�@@�K@@�@@��@?خ@?�[@?�	@?b�@>�'@>Q@=�@=�@=V@<bN@<@<G@;�;@;��@;iD@:�]@:��@:��@:J�@:�@9��@9X@8�f@8M@8@7�K@7s@7;d@6҉@6L0@5��@5�o@5Dg@4��@4�u@4�@333@2�6@2~�@2ff@2?@1@1�7@1p�@1o @1?}@0��@0��@02�@/��@/9�@/�@.��@.YK@.1�@.!�@-�@-x�@-A @-+@-%@,�p@,�@,tT@,U2@,<�@, �@+��@+9�@*��@*�y@*��@*{�@*!�@)��@)�z@)s�@)!�@(c�@(�@'��@'��@'�P@'iD@'E9@'&@' i@&�@&�b@&p;@&H�@&3�@&4@%��@%��@%rG@%<6@%!�@%+@$�[@$tT@$6@$6@$/�@$�@#�;@#�[@#�f@#S�@#(@"͟@"��@"� @"e@!�'@!\�@!+�@ ��@ ��@ �@ Q�@ "h@�}@�f@�@�R@��@3�@�@��@p�@(�@��@��@*�@ƨ@�P@9�@��@�@��@v�@Z�@($@��@�9@�-@J�@��@ѷ@��@�z@tT@�m@qv@)_@�@�<@��@Ov@0U@J@�j@��@�-@p�@�@��@�9@y>@-�@	�@�@� @�{@C@�@�,@��@�b@��@�@E�@ԕ@�M@hs@�P@�@�U@�@��@��@��@bN@� @�F@��@x@RT@+@��@��@��@u%@1�@
�@u@�T@�-@A @@@��@�@Xy@D�@1'@�@�0@��@�:@�f@RT@o@
�]@
�h@
�@
M�@
-1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��A��A�hA��A��A��A�A���A���A��A��+A��>A��rA��A��A�VA�\A�@A��A��A�!�A�&�A�'�A�&�A�*�A�2aA�49A�9$A�G�A�U�A�:*A��A��|AʣA�AǨ�A�b�A�-�AÿHAÌA�4nA���A��&A��A��A���A��hA���A��rA�xA�!�A�҉A��A�"�A�"�A�J�A��vA�cTA���A��&A��A���A�`vA��oA�(XA�3�A��A�1[A���A� 4A��A��A�E9A��gAƨAy�At��Aq	�Ak�6Af��Adu%AasA_�A\n�AZB�AWqAU��AU$tAS�VARB[AP��AN�AAK�0AI�_AGv�AFbNAE�KAD7LAA�nAA0UA@�A?�A=H�A=��A=�:A=9�A<��A:��A8	lA7�A6T�A0(�A.��A.�jA.ϫA.��A.;dA-�YA,��A,4�A+�A*H�A(��A(?A'�A'dZA&�A%�"A$�;A"VA!�A �!A!MA!��A"xlA"�yA"��A"�A!��A!J#A ��A ��A ƨA ��A ��A *�A zA :�AzxA<6A�A�NAzA��Ae�A�UAa|A�A�AiDA�8A��A^5A~A��AkQA�IAxA��A�YA��AA�A�QAeA��Ao�A1�A�A-A��AA�
AݘA�.A`�A?�A3�AGA�vA�wAZA+kA+A��A*�AƨA�"A�A�,A��A�4A*�A
=A��A�A�hAc A�WA��A�$AW?A
�<A
�nA
�!A
�A
�A
o�A
�A	�)A	�@A	s�A	;A�}A�AMA��A��Al�A��AU2A�A� A|�Al�A�AG�A
=AԕA�3A�[Ax�AMA��AxlA�)A �x@�0�@���@���@�^�@��@�%�@�;d@���@�u�@��;@�;@���@���@���@�K^@�_@�#�@��@���@��'@�>B@�f�@��@��@���@�bN@���@�(@꩓@ꅈ@�:*@���@�l�@�@�.@�&�@�v�@�3�@��z@�h
@�,�@��@��@�xl@�S�@��9@߂�@�5�@���@�D�@���@�%@�ff@���@�u�@���@�GE@�$t@أ@ש*@�7L@�֡@��.@Ԣ4@���@�&�@Ҟ�@��@ѩ�@�;d@�ں@�tT@ϐ�@��@���@Έ�@��K@�=@���@�R�@ˊ�@��@ʦL@�j@�*�@��#@�j�@�@Ȗ�@�@ǖS@�+�@�֡@�C�@��@Ű�@�4@Ğ@�)�@���@Ñh@�W?@��@�@��m@�+@���@�-@���@�6z@��9@�O@�Mj@��@�ی@�J�@��F@�8�@��@���@��I@��o@�Q�@�/�@��@�l�@�@��?@��L@�_�@�e@�u@���@�c�@���@���@��D@�~(@��9@���@�H@�~@��@���@�/�@��@���@�\�@�O@��V@�7L@��|@���@�]d@��N@��n@���@�� @�q�@�?@�@��C@��S@��~@�y�@�6z@��@�r�@�%�@��@��z@�o@��@���@�oi@�Q�@�6�@��@�ߤ@�W�@�ݘ@�zx@�C@���@���@�g8@���@�}�@�]�@�S�@��@��@�}V@�8�@��@��:@�U�@�.I@��@���@���@�5?@��@���@�a�@�Z�@�4�@�q@�
=@���@���@�@���@�P�@���@���@�[�@�V�@�E�@�.�@�x@��@���@���@�<6@��@�@���@���@���@���@��7@�o@���@�4@��#@�s�@�T�@�C�@��@���@���@�A�@��0@��7@��@��4@�Y�@��R@�Xy@�1�@��@�s@��	@��@�r�@�W�@�=q@��@��z@���@��"@��4@�{J@�%F@��@�YK@�.�@��@��N@���@�|@���@�z@��@��Z@���@���@��"@�dZ@�K�@�7L@��@���@���@�Z@��@��@���@�.I@��4@�W�@�7@���@���@�1�@�!�@��,@��j@��r@�M�@�=q@�*�@���@��@�k�@�H�@���@��@�B[@�  @���@�*0@��@��)@�^5@�$�@��3@��:@�j�@��@��@��B@��'@�:�@��t@��"@�e�@�S�@�Mj@�Dg@�:�@�.I@��v@�Ĝ@��@��r@�N�@��r@���@�m]@�X@�2a@��f@��X@���@�Q�@�/�@� @e�@8@�@~�\@~	@}�X@}o @}q@|��@|Q�@{ƨ@{_p@{6z@{�@z��@z�A@z;�@y��@y:�@x�[@xtT@w/�@vL0@v+k@v_@u�H@uVm@t�@t��@tD�@tb@s��@s�@sC�@ra|@r)�@r	@r	@q��@q�z@q��@p�v@o�0@n�8@nV@mϫ@m�=@mF@l�u@lN�@l1@k33@j�X@j��@j�b@j��@j1�@i��@i�h@h�@hZ@g��@gg�@f�8@fa|@e��@eVm@d�[@dD�@d�@c�g@c~�@c1�@b��@b��@b��@b^5@b.�@a��@a�t@a��@azx@aX@a8�@`�E@_U�@^��@^��@^J@]k�@\��@\�@\��@\��@\z�@\c�@\/�@[��@[�@[K�@[Mj@[A�@Z�"@Z�6@ZE�@Y��@Y5�@X��@X�Y@Wƨ@V��@V��@V�A@Vh
@VJ�@V)�@U��@U�N@U�@Uc�@U?}@U	l@T�O@TZ@T'R@S�@SX�@S6z@S�@RZ�@RJ@P��@PXy@P9X@O��@O�@NQ@M�T@M<6@L�f@Loi@K��@K�F@K~�@KS�@K!-@J�<@Jxl@I�@Ic@IB�@H�	@H�@HɆ@H��@H	�@Gg�@F��@F��@Fi�@F	@E�^@Em]@E8�@E�@D�I@D"h@C�@C�$@B�M@B��@B��@Ba|@B)�@B
�@A��@A�@Au�@AA @A�@@�K@@�@@��@?خ@?�[@?�	@?b�@>�'@>Q@=�@=�@=V@<bN@<@<G@;�;@;��@;iD@:�]@:��@:��@:J�@:�@9��@9X@8�f@8M@8@7�K@7s@7;d@6҉@6L0@5��@5�o@5Dg@4��@4�u@4�@333@2�6@2~�@2ff@2?@1@1�7@1p�@1o @1?}@0��@0��@02�@/��@/9�@/�@.��@.YK@.1�@.!�@-�@-x�@-A @-+@-%@,�p@,�@,tT@,U2@,<�@, �@+��@+9�@*��@*�y@*��@*{�@*!�@)��@)�z@)s�@)!�@(c�@(�@'��@'��@'�P@'iD@'E9@'&@' i@&�@&�b@&p;@&H�@&3�@&4@%��@%��@%rG@%<6@%!�@%+@$�[@$tT@$6@$6@$/�@$�@#�;@#�[@#�f@#S�@#(@"͟@"��@"� @"e@!�'@!\�@!+�@ ��@ ��@ �@ Q�@ "h@�}@�f@�@�R@��@3�@�@��@p�@(�@��@��@*�@ƨ@�P@9�@��@�@��@v�@Z�@($@��@�9@�-@J�@��@ѷ@��@�z@tT@�m@qv@)_@�@�<@��@Ov@0U@J@�j@��@�-@p�@�@��@�9@y>@-�@	�@�@� @�{@C@�@�,@��@�b@��@�@E�@ԕ@�M@hs@�P@�@�U@�@��@��@��@bN@� @�F@��@x@RT@+@��@��@��@u%@1�@
�@u@�T@�-@A @@@��@�@Xy@D�@1'@�@�0@��@�:@�f@RT@o@
�]@
�h@
�@
M�@
-1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B� B�bBЗBбB�HB�}B�.B�\B�BB��BЗB�BB��BбB�aB֡B�
B�$B�+B�QB��B�CB��B�B�/B��B� B�tB�8B�=B�B�B�B��B�_B��B�.B��BȀB
sMB
��B
�|B
�BUB�BI�BDBjBs�Bo5BpUBe�B��B��B��B�5B�uBpUB�vB�zB��B�sB�B6B�B
�B
��B
�hB
�PB
��B
xB
S[B
$B
uB
	�B	�B	�B	�nB	�~B	qB	X_B	OB	D3B	:*B	2B	+�B	"NB	�B	�B	�B	�B	�B	[B	,�B	I�B	M6B	UB	j�B	q[B	{JB	�HB	��B	�fB	��B	�ZB	�zB	�B	�B	�B	�qB	�;B	�qB	�B	�`B	��B	�B	��B
�B	��B	�B	�B	�wB	��B	��B	�B	�B	�oB	�B	�RB	�\B	ӏB	��B	�~B	ٴB	�5B	�}B
�B

B
1B
mB
SB
B
B
�B
OB
'8B
%�B
3�B
8�B
)�B
kB
7B
QB
B
�B
�B
�B
�B
�B
�B
SB
YB
�B
B
B
B
�B
$@B
8�B
A;B
EmB
@4B
EmB
G�B
CGB
?.B
>(B
=�B
="B
<�B
7�B
/iB
*B
:xB
@�B
@�B
C�B
IlB
I�B
H�B
HKB
H�B
H1B
G+B
H�B
G+B
C�B
@�B
>�B
<PB
:xB
:B
:*B
9�B
9�B
<PB
=�B
<�B
;�B
9�B
9>B
7B
5tB
5�B
8RB
9�B
:�B
=�B
@�B
@iB
>�B
>(B
=�B
;dB
9	B
5%B
0oB
-wB
,=B
-�B
*�B
'�B
*�B
0�B
1'B
2B
/5B
)DB
(�B
)�B
+B
*�B
)yB
(�B
&B
#�B
$�B
�B
�B
�B
 B
[B
�B
2B
�B
{B
�B
B
QB
eB
�B
�B
<B
	�B
YB
�B
dB
0B
6B
�B
PB
�B
BB
�B
VB
B
�B
"B
VB
pB
pB
VB
�B
�B
dB
�B
B

#B
�B
�B
�B
�B
B
zB
�B
+B
B
B
YB
?B
�B
9B
B
�B
B
B
[B
B
B
oB
UB
 iB
 OB
 �B
 �B
 OB
 �B
 �B
 �B
 �B
 �B
 �B
 iB
 B	��B	�.B	��B	�]B	��B	�wB	�wB	�wB	�]B	�wB	��B	�wB	��B	��B	��B	��B	��B	��B	��B	��B	�HB	�}B	��B	��B	��B	��B	��B	��B
 4B
 4B
 �B
 �B
 OB
 �B
 �B
oB
�B
�B
oB
B
AB
�B
uB
�B
�B
�B
�B
�B
�B
�B
MB
gB
gB
�B
�B
�B
�B
�B
%B
�B
�B
mB
?B
+B
�B
�B
B
_B
�B
�B
�B
�B
1B
1B
�B
�B
�B
KB
�B
�B

=B

	B
	�B

#B

	B

�B

�B

�B

�B
B
)B
dB
B
B
6B
pB
�B
�B
BB
BB
(B
B
�B
NB
�B
B
B
�B
�B
�B
B
�B
�B
NB
NB
�B
NB
4B
hB
:B
�B
&B
&B
&B
[B
�B
�B
B
FB
,B
aB
FB
FB
,B
�B
�B
2B
MB
�B
�B
�B
�B
�B
�B
B
B
�B
9B
B
9B
sB
YB
?B

B
�B
?B
$B
�B
EB
EB
�B
�B
B
yB
�B
�B
B
�B
�B
�B
�B
�B
xB
�B
xB
�B
�B
~B
�B
B
B
B
jB
�B
;B
!B
!B
�B
�B
�B
 B
 'B
 �B
 �B
 �B
!B
"B
"B
!�B
!�B
!�B
!�B
"B
#B
#TB
#�B
$&B
$@B
$�B
$�B
$�B
$�B
$�B
%B
%B
%�B
%�B
%�B
%�B
&2B
&B
&�B
&�B
'8B
'RB
'RB
'mB
'�B
($B
($B
(>B
(XB
(�B
)B
)B
)DB
*B
)�B
*KB
*�B
+B
+kB
+�B
+�B
,=B
,qB
,�B
,WB
-)B
-�B
-�B
.�B
.�B
.�B
.�B
.�B
.�B
/OB
/OB
/iB
/OB
/�B
0;B
0�B
1B
1B
1'B
1�B
1�B
1�B
2-B
2GB
2�B
33B
3B
33B
3�B
3�B
4B
49B
49B
4�B
4�B
5ZB
5�B
5�B
5�B
6B
6FB
6`B
6�B
72B
7�B
7fB
7LB
8B
8B
8lB
8�B
8�B
8lB
8RB
8�B
8�B
8�B
8lB
8�B
9rB
9rB
9rB
9rB
9rB
9rB
9XB
9�B
:�B
:�B
;dB
<B
<B
<6B
="B
="B
=qB
>BB
>�B
>�B
>�B
?B
?�B
?�B
@ B
@iB
@�B
AUB
AUB
A�B
BAB
B'B
B[B
B�B
C{B
C�B
C�B
C�B
D3B
DgB
DMB
D�B
D�B
D�B
EB
E9B
EB
EB
EB
EB
EB
E�B
E�B
E�B
F�B
G+B
GzB
G_B
G�B
G�B
G�B
G�B
G�B
G�B
HB
HKB
HfB
H�B
H�B
H�B
IB
IB
J	B
J#B
J	B
J�B
KB
KDB
KxB
KxB
KxB
K�B
K�B
K�B
K�B
LB
K�B
L0B
LdB
L�B
L�B
L�B
M6B
M6B
M6B
M�B
M�B
N�B
N�B
N�B
N�B
OvB
O�B
P.B
P�B
P�B
QB
QhB
Q�B
Q�B
Q�B
Q�B
RB
Q�B
R�B
R�B
R�B
SB
SB
SB
S@B
S[B
S�B
TaB
T,B
T�B
T�B
T�B
T�B
U2B
U2B
U�B
U�B
U�B
V9B
V�B
V�B
V�B
V�B
W$B
W
B
W$B
W?B
WsB
W�B
W�B
W�B
W�B
W�B
X�B
XyB
X�B
X�B
Y1B
YB
Y�B
Y�B
Z7B
[	B
[	B
[#B
[=B
[WB
[�B
\B
\)B
\)B
\]B
\xB
\�B
\�B
]/B
]�B
]�B
^B
^5B
^jB
^�B
_!B
_VB
_!B
_�B
`'B
`B
`vB
a-B
a|B
a�B
a�B
a�B
bB
bNB
bNB
b4B
bNB
b�B
b�B
cB
cTB
c�B
c�B
dB
dtB
dtB
dtB
d�B
d�B
e,B
eFB
e,B
ezB
e�B
e�B
e�B
e�B
e�B
fB
f�B
f�B
f�B
f�B
g8B
g�B
g�B
g�B
g�B
hXB
iB
i*B
iyB
iyB
i_B
iyB
iyB
i�B
i�B
i�B
i�B
i�B
j0B
j0B
jKB
jB
j�B
j�B
j�B
kB
k6B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
lB
l"B
l�B
l�B
l�B
l�B
m]B
m�B
nB
nB
ncB
n}B
n�B
n�B
n�B
o B
oB
o�B
o�B
o�B
p!B
pUB
p�B
p�B
p�B
qB
qB
q�B
q�B
q�B
rGB
r�B
r�B
r�B
r�B
sB
sB
shB
shB
shB
s�B
t9B
tTB
t9B
tTB
tnB
t�B
uZB
u�B
u�B
u�B
u�B
vFB
vFB
vzB
v�B
v�B
v�B
v�B
wLB
wfB
w�B
w�B
xB
xB
x8B
x8B
x�B
y	B
y	B
y>B
y>B
yrB
yrB
yXB
y�B
zB
z^B
z^B
{B
{0B
{0B
{0B
{0B
{0B
{0B
{B
|B
|6B
|PB
|jB
|�B
|�B
}"B
}<B
}VB
}VB
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
.B
HB
cB
cB
�B
� B
� B
� B
� B
�B
�iB
��B
��B
�B
�;B
�U1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B� B�bBЗBбB�HB�}B�.B�\B�BB��BЗB�BB��BбB�aB֡B�
B�$B�+B�QB��B�CB��B�B�/B��B� B�tB�8B�=B�B�B�B��B�_B��B�.B��BȀB
sMB
��B
�|B
�BUB�BI�BDBjBs�Bo5BpUBe�B��B��B��B�5B�uBpUB�vB�zB��B�sB�B6B�B
�B
��B
�hB
�PB
��B
xB
S[B
$B
uB
	�B	�B	�B	�nB	�~B	qB	X_B	OB	D3B	:*B	2B	+�B	"NB	�B	�B	�B	�B	�B	[B	,�B	I�B	M6B	UB	j�B	q[B	{JB	�HB	��B	�fB	��B	�ZB	�zB	�B	�B	�B	�qB	�;B	�qB	�B	�`B	��B	�B	��B
�B	��B	�B	�B	�wB	��B	��B	�B	�B	�oB	�B	�RB	�\B	ӏB	��B	�~B	ٴB	�5B	�}B
�B

B
1B
mB
SB
B
B
�B
OB
'8B
%�B
3�B
8�B
)�B
kB
7B
QB
B
�B
�B
�B
�B
�B
�B
SB
YB
�B
B
B
B
�B
$@B
8�B
A;B
EmB
@4B
EmB
G�B
CGB
?.B
>(B
=�B
="B
<�B
7�B
/iB
*B
:xB
@�B
@�B
C�B
IlB
I�B
H�B
HKB
H�B
H1B
G+B
H�B
G+B
C�B
@�B
>�B
<PB
:xB
:B
:*B
9�B
9�B
<PB
=�B
<�B
;�B
9�B
9>B
7B
5tB
5�B
8RB
9�B
:�B
=�B
@�B
@iB
>�B
>(B
=�B
;dB
9	B
5%B
0oB
-wB
,=B
-�B
*�B
'�B
*�B
0�B
1'B
2B
/5B
)DB
(�B
)�B
+B
*�B
)yB
(�B
&B
#�B
$�B
�B
�B
�B
 B
[B
�B
2B
�B
{B
�B
B
QB
eB
�B
�B
<B
	�B
YB
�B
dB
0B
6B
�B
PB
�B
BB
�B
VB
B
�B
"B
VB
pB
pB
VB
�B
�B
dB
�B
B

#B
�B
�B
�B
�B
B
zB
�B
+B
B
B
YB
?B
�B
9B
B
�B
B
B
[B
B
B
oB
UB
 iB
 OB
 �B
 �B
 OB
 �B
 �B
 �B
 �B
 �B
 �B
 iB
 B	��B	�.B	��B	�]B	��B	�wB	�wB	�wB	�]B	�wB	��B	�wB	��B	��B	��B	��B	��B	��B	��B	��B	�HB	�}B	��B	��B	��B	��B	��B	��B
 4B
 4B
 �B
 �B
 OB
 �B
 �B
oB
�B
�B
oB
B
AB
�B
uB
�B
�B
�B
�B
�B
�B
�B
MB
gB
gB
�B
�B
�B
�B
�B
%B
�B
�B
mB
?B
+B
�B
�B
B
_B
�B
�B
�B
�B
1B
1B
�B
�B
�B
KB
�B
�B

=B

	B
	�B

#B

	B

�B

�B

�B

�B
B
)B
dB
B
B
6B
pB
�B
�B
BB
BB
(B
B
�B
NB
�B
B
B
�B
�B
�B
B
�B
�B
NB
NB
�B
NB
4B
hB
:B
�B
&B
&B
&B
[B
�B
�B
B
FB
,B
aB
FB
FB
,B
�B
�B
2B
MB
�B
�B
�B
�B
�B
�B
B
B
�B
9B
B
9B
sB
YB
?B

B
�B
?B
$B
�B
EB
EB
�B
�B
B
yB
�B
�B
B
�B
�B
�B
�B
�B
xB
�B
xB
�B
�B
~B
�B
B
B
B
jB
�B
;B
!B
!B
�B
�B
�B
 B
 'B
 �B
 �B
 �B
!B
"B
"B
!�B
!�B
!�B
!�B
"B
#B
#TB
#�B
$&B
$@B
$�B
$�B
$�B
$�B
$�B
%B
%B
%�B
%�B
%�B
%�B
&2B
&B
&�B
&�B
'8B
'RB
'RB
'mB
'�B
($B
($B
(>B
(XB
(�B
)B
)B
)DB
*B
)�B
*KB
*�B
+B
+kB
+�B
+�B
,=B
,qB
,�B
,WB
-)B
-�B
-�B
.�B
.�B
.�B
.�B
.�B
.�B
/OB
/OB
/iB
/OB
/�B
0;B
0�B
1B
1B
1'B
1�B
1�B
1�B
2-B
2GB
2�B
33B
3B
33B
3�B
3�B
4B
49B
49B
4�B
4�B
5ZB
5�B
5�B
5�B
6B
6FB
6`B
6�B
72B
7�B
7fB
7LB
8B
8B
8lB
8�B
8�B
8lB
8RB
8�B
8�B
8�B
8lB
8�B
9rB
9rB
9rB
9rB
9rB
9rB
9XB
9�B
:�B
:�B
;dB
<B
<B
<6B
="B
="B
=qB
>BB
>�B
>�B
>�B
?B
?�B
?�B
@ B
@iB
@�B
AUB
AUB
A�B
BAB
B'B
B[B
B�B
C{B
C�B
C�B
C�B
D3B
DgB
DMB
D�B
D�B
D�B
EB
E9B
EB
EB
EB
EB
EB
E�B
E�B
E�B
F�B
G+B
GzB
G_B
G�B
G�B
G�B
G�B
G�B
G�B
HB
HKB
HfB
H�B
H�B
H�B
IB
IB
J	B
J#B
J	B
J�B
KB
KDB
KxB
KxB
KxB
K�B
K�B
K�B
K�B
LB
K�B
L0B
LdB
L�B
L�B
L�B
M6B
M6B
M6B
M�B
M�B
N�B
N�B
N�B
N�B
OvB
O�B
P.B
P�B
P�B
QB
QhB
Q�B
Q�B
Q�B
Q�B
RB
Q�B
R�B
R�B
R�B
SB
SB
SB
S@B
S[B
S�B
TaB
T,B
T�B
T�B
T�B
T�B
U2B
U2B
U�B
U�B
U�B
V9B
V�B
V�B
V�B
V�B
W$B
W
B
W$B
W?B
WsB
W�B
W�B
W�B
W�B
W�B
X�B
XyB
X�B
X�B
Y1B
YB
Y�B
Y�B
Z7B
[	B
[	B
[#B
[=B
[WB
[�B
\B
\)B
\)B
\]B
\xB
\�B
\�B
]/B
]�B
]�B
^B
^5B
^jB
^�B
_!B
_VB
_!B
_�B
`'B
`B
`vB
a-B
a|B
a�B
a�B
a�B
bB
bNB
bNB
b4B
bNB
b�B
b�B
cB
cTB
c�B
c�B
dB
dtB
dtB
dtB
d�B
d�B
e,B
eFB
e,B
ezB
e�B
e�B
e�B
e�B
e�B
fB
f�B
f�B
f�B
f�B
g8B
g�B
g�B
g�B
g�B
hXB
iB
i*B
iyB
iyB
i_B
iyB
iyB
i�B
i�B
i�B
i�B
i�B
j0B
j0B
jKB
jB
j�B
j�B
j�B
kB
k6B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
lB
l"B
l�B
l�B
l�B
l�B
m]B
m�B
nB
nB
ncB
n}B
n�B
n�B
n�B
o B
oB
o�B
o�B
o�B
p!B
pUB
p�B
p�B
p�B
qB
qB
q�B
q�B
q�B
rGB
r�B
r�B
r�B
r�B
sB
sB
shB
shB
shB
s�B
t9B
tTB
t9B
tTB
tnB
t�B
uZB
u�B
u�B
u�B
u�B
vFB
vFB
vzB
v�B
v�B
v�B
v�B
wLB
wfB
w�B
w�B
xB
xB
x8B
x8B
x�B
y	B
y	B
y>B
y>B
yrB
yrB
yXB
y�B
zB
z^B
z^B
{B
{0B
{0B
{0B
{0B
{0B
{0B
{B
|B
|6B
|PB
|jB
|�B
|�B
}"B
}<B
}VB
}VB
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
.B
HB
cB
cB
�B
� B
� B
� B
� B
�B
�iB
��B
��B
�B
�;B
�U1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105243  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604192407  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604192407  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604192407                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605042414  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605042414  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610161505                      G�O�G�O�G�O�                