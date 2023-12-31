CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:21:09Z creation;2022-06-04T19:21:09Z conversion to V3.1      
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
_FillValue                 �  I4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p|   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tl   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �X   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ޤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20220604192109  20220610151509  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               <A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�<=����1   @�<>ja�Q@-Y�+�coC��%1   GPS     A   B   B   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�33A�  A�  A�  A�  A�  A���B ffB  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B���B�33B���B�ffB�  B�  B�  B�  B�  B�ffB���B�  B�  B���B�  B�  B�  B�  B�  B���B�  B�  B�33B���B�  B�  B�  B�  B�  C   C  C�fC  C  C
  C  C  C  C  C  C  C  C  C33C�fC�fC!�fC#�fC&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF�CH  CJ  CL  CN  CPL�CQ�fCS�fCU�fCX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=fD=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�)�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @z�@z�H@�p�@�p�A�RA>�RA^�RA~�RA��\A�\)A�\)A�\)A�\)A�\)A�(�B zB�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B��
B��
B���B�
=B�p�B�=pB��
B��
B��
B��
B��
B�=pB���B��
B��
B�p�B��
B��
B��
B��
B��
Bף�B��
B��
B�
=B��B��
B��
B��
B��
B��
B��
C�C��C�C�C	�C�C�C�C�C�C�C�C�C�C��C��C!��C#��C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CFCG�CI�CK�CM�CP8RCQ��CS��CU��CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D	z�D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D�GD��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D#��D$z�D$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D=GD=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtz�Dt��Duz�Du��Dvz�Dv��Dwz�Dw��Dxz�Dx��Dyz�Dy��Dzz�Dz��D{z�D{��D|z�D|��D}z�D}��D~z�D~��Dz�D��D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�:>D�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD½qD��qD�=qD�}qDýqD��qD�=qD�}qDĽqD��qD�=qD�}qDŽqD��qD�=qD�}qDƽqD��qD�=qD�}qDǽqD��qD�=qD�}qDȽqD��qD�=qD�}qDɽqD��qD�=qD�}qDʽqD��qD�=qD�}qD˽qD��qD�=qD�}qD̽qD��qD�=qD�}qDͽqD��qD�=qD�}qDνqD��qD�=qD�}qDϽqD��qD�=qD�}qDнqD��qD�=qD�}qDѽqD��qD�=qD�}qDҽqD��qD�=qD�}qDӽqD��qD�=qD�}qDԽqD��qD�=qD�}qDսqD��qD�=qD�}qDֽqD��qD�=qD�}qD׽qD��qD�=qD�}qDؽqD��qD�=qD�}qDٽqD��qD�=qD�}qDڽqD��qD�=qD�}qD۽qD��qD�=qD�}qDܽqD��qD�=qD�}qDݽqD��qD�=qD�}qD޽qD��qD�=qD�}qD߽qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�'111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�wfA���A��mAږA�n/A�V�A�I�A�>�A�9XA�1�A�.IA�(�A�&�A�"�A�qA�SA���A���A�چA�ѷA�̘A���A٫6Aٔ�Aـ�A�o5A�a�A�\)A�MjA�7�A�+�A���A�ϫA�%�A�SA��vA̠'A���A�`BA�,qAĊ=A�J�A�o�A�"hA���A���A���A���A��sA��A�S�A���A���A�e�A�"�A��/A��A�@OA��AA�K�A��iA���A�RTA��hA�&A�
	A�UgA��8A��'A�B�A��VA���A�:*A�XA��A�[#A�Z�A~HA{J�An�dAiN�Ad��Aa��A_~A\
�AY�9AX��AW�,AU��AR0�APW?ANC�ALPHAJH�AIAG4�AD
=AB��A@��A?�A=�A<�A9H�A7�A6�zA5U2A41�A2�A1�A10UA1  A0r�A.��A-ϫA-"hA,5?A+ݘA+��A*�+A)��A(%FA'FA$خA#�4A"�"A"��A!�DA!{JA ��A �A MjAخAh�AT�A�	A��A�IAxlAJA�^A��AQ�A�A��A��A�AR�A��A�kAxA �A��AOvA��A%A��A�=A|�A��AJA�Af�Av`A
�8AIRA
��A	��A�'A�wAhsA�A��A �<@���@�A�@�G@�l�@�_@�+@���@�\)@�u�@�s�@�)_@�u%@���@��x@�*�@��W@��m@���@��@�1�@�+@�o�@�v�@���@��o@��@@��H@��@��|@�@�/@�+@��@��@�_p@�~(@�1'@�o@�{@��@�l"@��j@��@��-@�7@�Q�@��m@�e�@��@�[W@��@�.@��@��@���@詓@�>B@�  @��d@��|@��@���@��@���@�L@��a@�2a@���@ℶ@�@���@�D�@��@���@�4@�;@޵@��}@܌@�?�@�x@���@�kQ@��@م@��	@�9X@��A@��@�Ft@ռ@�F�@���@ԑ @�ƨ@�J#@��@Ҍ@��#@п�@��@�\)@��@��@Ͳ�@�p�@�C@̾�@̂A@�j@�[�@�C-@���@�j@��8@ʴ9@�;�@�@O@��X@�xl@�/�@���@�H�@��|@�=q@Ų�@�j�@�l�@�2a@��@´9@�L0@��K@��{@�~�@���@���@�s�@�ی@���@�]d@��@���@��q@� \@�  @��)@���@�s@�2a@��@�-@�Y�@�R�@��;@�^�@��@��6@�}V@�V@�I�@�$�@���@��n@�a@�-w@��c@��9@��@�Z@�e@�خ@��{@�\)@�S&@�L�@�B�@�"�@��K@���@���@�|�@�x@�n/@��@�҉@�Ĝ@��@�L0@�($@���@���@��B@�oi@�I�@��9@�Mj@���@��,@�GE@�;@���@��@�'R@�ݘ@���@�e�@�33@��@��p@�q�@�*�@��Q@���@��C@�xl@��@��}@��C@�X�@��@��b@�Z�@���@�l�@��@���@�a|@��@��"@�X@��@���@��@�S�@��@���@��H@���@�{J@�@��@���@�;�@�  @���@���@�8�@��@�ѷ@��o@�Xy@�b@��@�\�@�+@��@���@��L@���@�A�@�  @���@��	@�e,@�O�@�=�@��f@��z@�V@�6�@�!�@��A@���@��	@�=�@���@�s�@�U2@�G@�H�@���@��I@�Ta@�M@�@�j@�B�@���@�6�@��@�j@�=@�V@��<@�tT@�@�ԕ@��^@��@�j�@�,�@��	@��@�GE@��o@��9@���@�g�@��@���@���@��Z@�f�@�@@�Ɇ@�tT@�M@�@��}@���@�ƨ@��@���@�w2@�@O@���@���@�j@�?�@�4@��q@�X@�/@��@��f@�ȴ@��@�)�@��m@��C@�w2@�`B@�8�@��@��"@���@���@�� @�l�@�E�@���@��
@�x�@�(@���@���@��H@��e@�YK@�G@��g@���@���@�&�@�@��@�;@���@��@���@��o@�n�@�)�@�6@F�@~{�@}��@}#�@|�p@|~(@|�@{ݘ@{v`@z��@z��@z@y��@y�S@yVm@x��@w|�@vff@u�@t��@t�@tm�@t*�@s�a@sX�@s9�@sC@r��@rR�@qx�@q�@p��@p@o�6@oE9@n��@nȴ@n�r@m�M@l�@l�@k�A@k�P@kE9@j��@jJ�@i�D@iL�@h�@h[�@h(�@g˒@g"�@f�<@f��@f{�@fl�@fO@e��@eIR@e7L@e@d�9@d��@d7�@c��@c�*@cP�@b��@b�x@bxl@bOv@b@aϫ@a�M@a�@`r�@`~@_��@_;d@^�@]��@]5�@\Ɇ@\�Y@\U2@\�@[�@[�K@[�@[a@[.I@[
=@Z�X@Z_�@Y�@Yj@YX@Y@X֡@X�.@X-�@W�}@W�*@W,�@Vں@V��@V4@U#�@T�5@T��@TI�@T�@S��@S��@Sƨ@S�:@St�@Sl�@S\)@SY@RM�@Q�@Q�3@Q5�@P�E@PXy@O�]@O�[@OdZ@O�@N��@N�\@N3�@M�9@MA @L��@L�z@LN�@K�@K��@K��@KS@JC�@I��@I�-@I�@IDg@H�/@Hu�@H~@G��@G�4@G�@F��@FOv@E�o@E�d@E��@E<6@D�@D�@De�@DPH@D<�@D �@C�@B�'@B�+@A�@@�@@��@@D�@?�@?|�@?F�@?&@>�@>��@>W�@=�)@=��@=X@= \@<�$@<�.@<�o@<_@<~@;�F@:�y@:p;@9�@9��@9B�@9�@8��@7��@7��@7X�@7K�@76z@7�@6a|@5�@50�@5�@4�z@4'R@3�w@3j�@3�@2��@2��@2s�@2C�@1�@1��@1T�@0��@0�D@0V�@/��@/��@/RT@/�@.�\@.C�@.+k@-�@-a�@-!�@,��@,y>@,2�@+�@+X�@+@*�}@*q�@*GE@*($@)�9@)-w@(�	@(�@(�U@(u�@(`�@(H@(�@'�6@'e�@&�8@&�<@&Z�@&@%�@%�h@%F@%+�@%#�@%�@$�/@$�@$�@$j@$V�@#˒@#��@#��@#C�@"��@"��@"�2@"͟@"kQ@"_@!�j@!�@!��@!B�@ ��@ 7�@�$@v`@\)@33@Y@�@�@�@z@+k@	@�j@��@x�@?}@0�@�@�@��@�Y@[�@C-@,=@�@�A@��@�:@/�@ i@��@Ov@?@�@��@��@j@N<@�P@�@~(@�@��@�0@�4@\)@F�@�@�F@0U@
�@�@@�t@��@�@�@�/@��@�j@�@�@�.@�@h�@4n@�@�@O@F�@A�@;d@8@/�@(@͟@�1@v�@�@��@�C@��@x�@�@�5@Ĝ@��@r�@I�@�@��@|�@S�@&@ߤ@��@��@xl@L0@6�@	@�@�-@rG@X@IR@F@/@@�@�@�4@Z@(�@��@�Q@�@��@v`@=@+@�@�@
��@
�L@
� @
� @
��@
v�@
M�@
@	�@	�@	�M@	`B@	&�@	q@	�@�/@Ɇ@��@�@>B@1@�@�
@�a@��@�q@�P@F�@"�@�]@�}@� @��@��@a|@8�@e@�z@��@c@T�@IR@/@�@�@�P@�@�[@Ĝ@��111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�wfA���A��mAږA�n/A�V�A�I�A�>�A�9XA�1�A�.IA�(�A�&�A�"�A�qA�SA���A���A�چA�ѷA�̘A���A٫6Aٔ�Aـ�A�o5A�a�A�\)A�MjA�7�A�+�A���A�ϫA�%�A�SA��vA̠'A���A�`BA�,qAĊ=A�J�A�o�A�"hA���A���A���A���A��sA��A�S�A���A���A�e�A�"�A��/A��A�@OA��AA�K�A��iA���A�RTA��hA�&A�
	A�UgA��8A��'A�B�A��VA���A�:*A�XA��A�[#A�Z�A~HA{J�An�dAiN�Ad��Aa��A_~A\
�AY�9AX��AW�,AU��AR0�APW?ANC�ALPHAJH�AIAG4�AD
=AB��A@��A?�A=�A<�A9H�A7�A6�zA5U2A41�A2�A1�A10UA1  A0r�A.��A-ϫA-"hA,5?A+ݘA+��A*�+A)��A(%FA'FA$خA#�4A"�"A"��A!�DA!{JA ��A �A MjAخAh�AT�A�	A��A�IAxlAJA�^A��AQ�A�A��A��A�AR�A��A�kAxA �A��AOvA��A%A��A�=A|�A��AJA�Af�Av`A
�8AIRA
��A	��A�'A�wAhsA�A��A �<@���@�A�@�G@�l�@�_@�+@���@�\)@�u�@�s�@�)_@�u%@���@��x@�*�@��W@��m@���@��@�1�@�+@�o�@�v�@���@��o@��@@��H@��@��|@�@�/@�+@��@��@�_p@�~(@�1'@�o@�{@��@�l"@��j@��@��-@�7@�Q�@��m@�e�@��@�[W@��@�.@��@��@���@詓@�>B@�  @��d@��|@��@���@��@���@�L@��a@�2a@���@ℶ@�@���@�D�@��@���@�4@�;@޵@��}@܌@�?�@�x@���@�kQ@��@م@��	@�9X@��A@��@�Ft@ռ@�F�@���@ԑ @�ƨ@�J#@��@Ҍ@��#@п�@��@�\)@��@��@Ͳ�@�p�@�C@̾�@̂A@�j@�[�@�C-@���@�j@��8@ʴ9@�;�@�@O@��X@�xl@�/�@���@�H�@��|@�=q@Ų�@�j�@�l�@�2a@��@´9@�L0@��K@��{@�~�@���@���@�s�@�ی@���@�]d@��@���@��q@� \@�  @��)@���@�s@�2a@��@�-@�Y�@�R�@��;@�^�@��@��6@�}V@�V@�I�@�$�@���@��n@�a@�-w@��c@��9@��@�Z@�e@�خ@��{@�\)@�S&@�L�@�B�@�"�@��K@���@���@�|�@�x@�n/@��@�҉@�Ĝ@��@�L0@�($@���@���@��B@�oi@�I�@��9@�Mj@���@��,@�GE@�;@���@��@�'R@�ݘ@���@�e�@�33@��@��p@�q�@�*�@��Q@���@��C@�xl@��@��}@��C@�X�@��@��b@�Z�@���@�l�@��@���@�a|@��@��"@�X@��@���@��@�S�@��@���@��H@���@�{J@�@��@���@�;�@�  @���@���@�8�@��@�ѷ@��o@�Xy@�b@��@�\�@�+@��@���@��L@���@�A�@�  @���@��	@�e,@�O�@�=�@��f@��z@�V@�6�@�!�@��A@���@��	@�=�@���@�s�@�U2@�G@�H�@���@��I@�Ta@�M@�@�j@�B�@���@�6�@��@�j@�=@�V@��<@�tT@�@�ԕ@��^@��@�j�@�,�@��	@��@�GE@��o@��9@���@�g�@��@���@���@��Z@�f�@�@@�Ɇ@�tT@�M@�@��}@���@�ƨ@��@���@�w2@�@O@���@���@�j@�?�@�4@��q@�X@�/@��@��f@�ȴ@��@�)�@��m@��C@�w2@�`B@�8�@��@��"@���@���@�� @�l�@�E�@���@��
@�x�@�(@���@���@��H@��e@�YK@�G@��g@���@���@�&�@�@��@�;@���@��@���@��o@�n�@�)�@�6@F�@~{�@}��@}#�@|�p@|~(@|�@{ݘ@{v`@z��@z��@z@y��@y�S@yVm@x��@w|�@vff@u�@t��@t�@tm�@t*�@s�a@sX�@s9�@sC@r��@rR�@qx�@q�@p��@p@o�6@oE9@n��@nȴ@n�r@m�M@l�@l�@k�A@k�P@kE9@j��@jJ�@i�D@iL�@h�@h[�@h(�@g˒@g"�@f�<@f��@f{�@fl�@fO@e��@eIR@e7L@e@d�9@d��@d7�@c��@c�*@cP�@b��@b�x@bxl@bOv@b@aϫ@a�M@a�@`r�@`~@_��@_;d@^�@]��@]5�@\Ɇ@\�Y@\U2@\�@[�@[�K@[�@[a@[.I@[
=@Z�X@Z_�@Y�@Yj@YX@Y@X֡@X�.@X-�@W�}@W�*@W,�@Vں@V��@V4@U#�@T�5@T��@TI�@T�@S��@S��@Sƨ@S�:@St�@Sl�@S\)@SY@RM�@Q�@Q�3@Q5�@P�E@PXy@O�]@O�[@OdZ@O�@N��@N�\@N3�@M�9@MA @L��@L�z@LN�@K�@K��@K��@KS@JC�@I��@I�-@I�@IDg@H�/@Hu�@H~@G��@G�4@G�@F��@FOv@E�o@E�d@E��@E<6@D�@D�@De�@DPH@D<�@D �@C�@B�'@B�+@A�@@�@@��@@D�@?�@?|�@?F�@?&@>�@>��@>W�@=�)@=��@=X@= \@<�$@<�.@<�o@<_@<~@;�F@:�y@:p;@9�@9��@9B�@9�@8��@7��@7��@7X�@7K�@76z@7�@6a|@5�@50�@5�@4�z@4'R@3�w@3j�@3�@2��@2��@2s�@2C�@1�@1��@1T�@0��@0�D@0V�@/��@/��@/RT@/�@.�\@.C�@.+k@-�@-a�@-!�@,��@,y>@,2�@+�@+X�@+@*�}@*q�@*GE@*($@)�9@)-w@(�	@(�@(�U@(u�@(`�@(H@(�@'�6@'e�@&�8@&�<@&Z�@&@%�@%�h@%F@%+�@%#�@%�@$�/@$�@$�@$j@$V�@#˒@#��@#��@#C�@"��@"��@"�2@"͟@"kQ@"_@!�j@!�@!��@!B�@ ��@ 7�@�$@v`@\)@33@Y@�@�@�@z@+k@	@�j@��@x�@?}@0�@�@�@��@�Y@[�@C-@,=@�@�A@��@�:@/�@ i@��@Ov@?@�@��@��@j@N<@�P@�@~(@�@��@�0@�4@\)@F�@�@�F@0U@
�@�@@�t@��@�@�@�/@��@�j@�@�@�.@�@h�@4n@�@�@O@F�@A�@;d@8@/�@(@͟@�1@v�@�@��@�C@��@x�@�@�5@Ĝ@��@r�@I�@�@��@|�@S�@&@ߤ@��@��@xl@L0@6�@	@�@�-@rG@X@IR@F@/@@�@�@�4@Z@(�@��@�Q@�@��@v`@=@+@�@�@
��@
�L@
� @
� @
��@
v�@
M�@
@	�@	�@	�M@	`B@	&�@	q@	�@�/@Ɇ@��@�@>B@1@�@�
@�a@��@�q@�P@F�@"�@�]@�}@� @��@��@a|@8�@e@�z@��@c@T�@IR@/@�@�@�P@�@�[@Ĝ@��111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	��B	�B	�B	��B	��B	�[B	�'B	�AB	�AB	�AB	�vB	�B	��B	�B	��B	�vB	�B	�|B	�B	��B	�%B	�?B	��B	�B	��B	�B	�B	��B	�$B	�XB	��B	��B	�XB	�B	�B	�<B	�#B	�'B	��B	�BB	��B	�]B	�JB	�_B	�HB
eB
8�B
z�B
{�B
��B
��B
��B
�3B
�TB)*B$@B1ABX�BgRB��B��B�BG�B1AB8�BuB
� B
՛B
��B
��B
��B
n}B
LJB
:^B
4nB
*B
QB	��B	߾B	�tB	_pB	<�B	%�B	gB	�B�DB��B�iB��B�BՁB̘B�1B��B��B��B�kB��B��B�B��B�5B��BɠB�7B��B��B	�B		B	�B	OB	 �B	+�B	5�B	<�B	LB	O�B	Q B	W�B	a|B	jeB	n�B	ezB	_�B	_�B	a�B	c�B	d�B	ffB	gB	hXB	jB	jB	jB	mCB	r�B	u?B	{0B	~�B	��B	�B	��B	�#B	�B	��B	�@B	�4B	��B	��B	�>B	�LB	�jB	��B	�$B	��B	�CB	�4B	��B	��B	��B	�B	�0B	��B	h>B	m�B	yXB	i�B	b�B	R�B	=�B	1�B	,�B	'8B	�B	9B	%B	*�B	/�B	5�B	5B	4B	33B	3�B	4�B	7LB	>BB	GzB	IlB	JrB	K�B	R:B	_�B	j�B	o�B	lWB	i�B	h$B	h$B	e�B	m�B	�-B	��B	��B	�LB	�MB	�9B	�tB	�RB	�vB	� B	ȚB	�B	�B	��B	ŢB	��B	�rB	�xB	�VB	ѝB	��B	ԕB	�2B	�YB	��B	�KB	�B	ݲB	�OB	ߤB	߾B	ߤB	�;B	�VB	�!B	��B	�;B	��B	��B	��B	��B	��B	�bB	��B	�vB	�\B	�B	��B	��B	�B	��B	��B	�B	�8B	�
B	�
B	��B	�B	�mB	�B	��B	�B	��B	��B	��B	��B	��B	��B	�_B	�_B	�_B	��B	��B	�B	��B	�yB	�_B	�B	��B	�=B	�=B	�B	��B	��B	��B	��B	��B	�cB	�B	�oB	�B	��B	��B	�3B	��B	�B	�B	�MB	�9B	�TB	�TB	��B	�B	�B	�3B	�hB	��B	�B	�GB	�AB	��B	��B	�B	�MB	�B	�?B	��B	�B	�FB	�B	��B	�?B	�%B	��B	��B	��B	�B	��B	��B	��B	�B	�8B	��B	��B	��B	�"B	��B	��B	�HB	��B
  B
 OB
 iB
 �B
 B
�B
�B
GB
aB
aB
aB
{B
�B
�B
MB
gB
�B
B
 �B
  B
 B
B
�B
YB
�B
�B
�B
zB
EB
�B
SB
�B
�B
�B
�B
B
{B
B
�B
�B
�B
�B
MB
MB
B
SB
�B
�B
YB
�B
�B
�B
�B
B
B
mB
mB
tB
�B
�B
�B
�B
zB
�B
�B
�B
�B
�B

	B

XB

#B
^B
�B
0B
�B
dB
B
�B
�B
pB
�B
B
B
"B
"B
B
�B
(B
bB
�B
 B
NB
�B
�B
B
�B
�B
:B
[B
�B
FB
�B
�B
�B
�B
�B
�B
2B
�B
2B
�B
SB
�B
�B
mB
B
�B
mB
sB
�B
�B
�B
�B
#B
�B
jB
jB
jB
�B
�B
�B
�B
�B
�B
 B
 B
�B
 �B
 �B
!�B
!�B
!�B
"�B
"NB
!|B
 �B
 �B
 �B
!|B
!�B
"B
!�B
"�B
# B
$ZB
$�B
$�B
$�B
$�B
$�B
%B
$�B
%FB
%`B
%�B
&�B
'�B
(
B
(XB
(�B
)B
)�B
)�B
*B
*eB
*�B
+B
+6B
+�B
+�B
+�B
,"B
,�B
,�B
,�B
-]B
-]B
.IB
.�B
.�B
.�B
.�B
/�B
0!B
0oB
0�B
1'B
1AB
1vB
1vB
1vB
1vB
1[B
1[B
2B
2|B
2|B
2�B
2�B
2�B
2�B
3hB
49B
49B
4nB
4�B
5%B
5tB
6B
6B
6zB
6�B
6�B
6�B
7B
7�B
8�B
8�B
9�B
9�B
9�B
9�B
:B
:^B
:^B
:^B
:DB
:�B
:�B
;0B
;dB
;�B
;�B
<B
<B
<B
<B
<�B
=B
="B
=�B
=�B
=�B
=�B
>]B
>wB
>�B
?B
?}B
?�B
?�B
@�B
@�B
@�B
@�B
@�B
@�B
A B
A;B
AB
@�B
AUB
AoB
A�B
A�B
A�B
BB
BAB
BuB
B�B
B�B
B�B
B�B
CB
CB
C-B
C{B
CaB
CaB
C�B
DMB
EB
E�B
E�B
FB
F?B
F?B
FtB
F�B
F�B
F�B
F�B
F�B
GEB
G�B
HB
HB
H1B
HfB
H�B
H�B
I7B
I7B
IB
IRB
IB
IlB
IlB
I7B
I�B
I�B
I�B
J	B
J	B
J#B
JXB
JXB
J=B
J=B
JXB
K^B
K�B
K�B
L�B
L�B
MPB
M�B
M�B
M�B
M�B
M�B
N<B
NpB
N�B
N�B
OB
OB
O\B
OvB
OvB
OvB
O�B
P.B
P�B
P�B
P�B
P�B
Q B
QNB
Q�B
Q�B
Q�B
R B
R�B
R�B
R�B
R�B
R�B
S@B
S[B
S�B
S�B
S�B
S�B
S[B
T�B
T{B
T�B
UgB
U�B
VB
VB
VSB
V�B
V�B
V�B
W$B
W$B
WsB
W�B
W�B
XEB
XEB
X�B
X�B
X�B
X�B
X�B
YB
Y�B
ZB
Z�B
Z�B
[=B
[=B
[�B
[�B
\]B
\�B
\�B
\�B
\�B
]~B
]�B
^jB
^jB
^�B
^�B
_B
_VB
_�B
_�B
_�B
_�B
`B
`\B
`�B
aB
abB
a�B
a�B
a�B
a�B
bNB
bhB
c B
c B
c B
cTB
c�B
c�B
d&B
d&B
dZB
d�B
eB
eFB
e�B
e�B
e�B
e�B
e�B
f�B
f�B
f�B
f�B
gB
gB
gB
gRB
gmB
g�B
h
B
h>B
hsB
h�B
h�B
iB
i_B
iyB
i_B
i�B
i�B
i�B
i�B
i�B
i�B
j�B
jB
j�B
kB
kQB
kQB
k6B
kkB
k�B
lB
l�B
mB
mCB
m]B
m�B
nB
ncB
nIB
ncB
n}B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
oB
oOB
oiB
o�B
o�B
o�B
o�B
p;B
poB
p�B
poB
p�B
p�B
qB
q[B
q�B
r-B
r-B
r|B
s3B
sB
sMB
s�B
s�B
s�B
s�B
t9B
t9B
t�B
uB
uB
uZB
utB
utB
uZB
u�B
u�B
vFB
vFB
v�B
v�B
v�B
v�B
wB
w2B
wLB
wfB
wfB
w�B
w�B
w�B
w�B
w�B
w�B
w�B
xRB
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y	B
y	B
y�B
y�B
y�B
y�B
zB
z�B
z�B
z�B
{0B
{JB
{JB
{�B
{�B
|B
|B
|6B
|jB
|�B
|�B
|�B
|�B
|�B
|�B
}<B
}VB
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~B
~BB
~wB
~�B
~�B
~�B
~�B
.B
.B
}B
}B
}B
�B
�B
�B
�B
�B
�B
�B
�OB
��B
��B
� B
� B
�UB
��B
��B
��B
��B
��B
�'B
�AB
��B
��B
��B
��B
�B
�B
�B
�-B
��B
�{B
��B
�B
�MB
�gB
�3B
��B
�gB
��B
�B
�B
�9B
��B
��B
��B
��B
��B
��B
��B
�?B
�?B
�?111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	��B	�B	�B	��B	��B	�[B	�'B	�AB	�AB	�AB	�vB	�B	��B	�B	��B	�vB	�B	�|B	�B	��B	�%B	�?B	��B	�B	��B	�B	�B	��B	�$B	�XB	��B	��B	�XB	�B	�B	�<B	�#B	�'B	��B	�BB	��B	�]B	�JB	�_B	�HB
eB
8�B
z�B
{�B
��B
��B
��B
�3B
�TB)*B$@B1ABX�BgRB��B��B�BG�B1AB8�BuB
� B
՛B
��B
��B
��B
n}B
LJB
:^B
4nB
*B
QB	��B	߾B	�tB	_pB	<�B	%�B	gB	�B�DB��B�iB��B�BՁB̘B�1B��B��B��B�kB��B��B�B��B�5B��BɠB�7B��B��B	�B		B	�B	OB	 �B	+�B	5�B	<�B	LB	O�B	Q B	W�B	a|B	jeB	n�B	ezB	_�B	_�B	a�B	c�B	d�B	ffB	gB	hXB	jB	jB	jB	mCB	r�B	u?B	{0B	~�B	��B	�B	��B	�#B	�B	��B	�@B	�4B	��B	��B	�>B	�LB	�jB	��B	�$B	��B	�CB	�4B	��B	��B	��B	�B	�0B	��B	h>B	m�B	yXB	i�B	b�B	R�B	=�B	1�B	,�B	'8B	�B	9B	%B	*�B	/�B	5�B	5B	4B	33B	3�B	4�B	7LB	>BB	GzB	IlB	JrB	K�B	R:B	_�B	j�B	o�B	lWB	i�B	h$B	h$B	e�B	m�B	�-B	��B	��B	�LB	�MB	�9B	�tB	�RB	�vB	� B	ȚB	�B	�B	��B	ŢB	��B	�rB	�xB	�VB	ѝB	��B	ԕB	�2B	�YB	��B	�KB	�B	ݲB	�OB	ߤB	߾B	ߤB	�;B	�VB	�!B	��B	�;B	��B	��B	��B	��B	��B	�bB	��B	�vB	�\B	�B	��B	��B	�B	��B	��B	�B	�8B	�
B	�
B	��B	�B	�mB	�B	��B	�B	��B	��B	��B	��B	��B	��B	�_B	�_B	�_B	��B	��B	�B	��B	�yB	�_B	�B	��B	�=B	�=B	�B	��B	��B	��B	��B	��B	�cB	�B	�oB	�B	��B	��B	�3B	��B	�B	�B	�MB	�9B	�TB	�TB	��B	�B	�B	�3B	�hB	��B	�B	�GB	�AB	��B	��B	�B	�MB	�B	�?B	��B	�B	�FB	�B	��B	�?B	�%B	��B	��B	��B	�B	��B	��B	��B	�B	�8B	��B	��B	��B	�"B	��B	��B	�HB	��B
  B
 OB
 iB
 �B
 B
�B
�B
GB
aB
aB
aB
{B
�B
�B
MB
gB
�B
B
 �B
  B
 B
B
�B
YB
�B
�B
�B
zB
EB
�B
SB
�B
�B
�B
�B
B
{B
B
�B
�B
�B
�B
MB
MB
B
SB
�B
�B
YB
�B
�B
�B
�B
B
B
mB
mB
tB
�B
�B
�B
�B
zB
�B
�B
�B
�B
�B

	B

XB

#B
^B
�B
0B
�B
dB
B
�B
�B
pB
�B
B
B
"B
"B
B
�B
(B
bB
�B
 B
NB
�B
�B
B
�B
�B
:B
[B
�B
FB
�B
�B
�B
�B
�B
�B
2B
�B
2B
�B
SB
�B
�B
mB
B
�B
mB
sB
�B
�B
�B
�B
#B
�B
jB
jB
jB
�B
�B
�B
�B
�B
�B
 B
 B
�B
 �B
 �B
!�B
!�B
!�B
"�B
"NB
!|B
 �B
 �B
 �B
!|B
!�B
"B
!�B
"�B
# B
$ZB
$�B
$�B
$�B
$�B
$�B
%B
$�B
%FB
%`B
%�B
&�B
'�B
(
B
(XB
(�B
)B
)�B
)�B
*B
*eB
*�B
+B
+6B
+�B
+�B
+�B
,"B
,�B
,�B
,�B
-]B
-]B
.IB
.�B
.�B
.�B
.�B
/�B
0!B
0oB
0�B
1'B
1AB
1vB
1vB
1vB
1vB
1[B
1[B
2B
2|B
2|B
2�B
2�B
2�B
2�B
3hB
49B
49B
4nB
4�B
5%B
5tB
6B
6B
6zB
6�B
6�B
6�B
7B
7�B
8�B
8�B
9�B
9�B
9�B
9�B
:B
:^B
:^B
:^B
:DB
:�B
:�B
;0B
;dB
;�B
;�B
<B
<B
<B
<B
<�B
=B
="B
=�B
=�B
=�B
=�B
>]B
>wB
>�B
?B
?}B
?�B
?�B
@�B
@�B
@�B
@�B
@�B
@�B
A B
A;B
AB
@�B
AUB
AoB
A�B
A�B
A�B
BB
BAB
BuB
B�B
B�B
B�B
B�B
CB
CB
C-B
C{B
CaB
CaB
C�B
DMB
EB
E�B
E�B
FB
F?B
F?B
FtB
F�B
F�B
F�B
F�B
F�B
GEB
G�B
HB
HB
H1B
HfB
H�B
H�B
I7B
I7B
IB
IRB
IB
IlB
IlB
I7B
I�B
I�B
I�B
J	B
J	B
J#B
JXB
JXB
J=B
J=B
JXB
K^B
K�B
K�B
L�B
L�B
MPB
M�B
M�B
M�B
M�B
M�B
N<B
NpB
N�B
N�B
OB
OB
O\B
OvB
OvB
OvB
O�B
P.B
P�B
P�B
P�B
P�B
Q B
QNB
Q�B
Q�B
Q�B
R B
R�B
R�B
R�B
R�B
R�B
S@B
S[B
S�B
S�B
S�B
S�B
S[B
T�B
T{B
T�B
UgB
U�B
VB
VB
VSB
V�B
V�B
V�B
W$B
W$B
WsB
W�B
W�B
XEB
XEB
X�B
X�B
X�B
X�B
X�B
YB
Y�B
ZB
Z�B
Z�B
[=B
[=B
[�B
[�B
\]B
\�B
\�B
\�B
\�B
]~B
]�B
^jB
^jB
^�B
^�B
_B
_VB
_�B
_�B
_�B
_�B
`B
`\B
`�B
aB
abB
a�B
a�B
a�B
a�B
bNB
bhB
c B
c B
c B
cTB
c�B
c�B
d&B
d&B
dZB
d�B
eB
eFB
e�B
e�B
e�B
e�B
e�B
f�B
f�B
f�B
f�B
gB
gB
gB
gRB
gmB
g�B
h
B
h>B
hsB
h�B
h�B
iB
i_B
iyB
i_B
i�B
i�B
i�B
i�B
i�B
i�B
j�B
jB
j�B
kB
kQB
kQB
k6B
kkB
k�B
lB
l�B
mB
mCB
m]B
m�B
nB
ncB
nIB
ncB
n}B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
oB
oOB
oiB
o�B
o�B
o�B
o�B
p;B
poB
p�B
poB
p�B
p�B
qB
q[B
q�B
r-B
r-B
r|B
s3B
sB
sMB
s�B
s�B
s�B
s�B
t9B
t9B
t�B
uB
uB
uZB
utB
utB
uZB
u�B
u�B
vFB
vFB
v�B
v�B
v�B
v�B
wB
w2B
wLB
wfB
wfB
w�B
w�B
w�B
w�B
w�B
w�B
w�B
xRB
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y	B
y	B
y�B
y�B
y�B
y�B
zB
z�B
z�B
z�B
{0B
{JB
{JB
{�B
{�B
|B
|B
|6B
|jB
|�B
|�B
|�B
|�B
|�B
|�B
}<B
}VB
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~B
~BB
~wB
~�B
~�B
~�B
~�B
.B
.B
}B
}B
}B
�B
�B
�B
�B
�B
�B
�B
�OB
��B
��B
� B
� B
�UB
��B
��B
��B
��B
��B
�'B
�AB
��B
��B
��B
��B
�B
�B
�B
�-B
��B
�{B
��B
�B
�MB
�gB
�3B
��B
�gB
��B
�B
�B
�9B
��B
��B
��B
��B
��B
��B
��B
�?B
�?B
�?111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105240  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604192109  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604192109  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604192109                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605042117  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605042117  QCF$                G�O�G�O�G�O�            4000JA  ARUP                                                                        20220610151509                      G�O�G�O�G�O�                