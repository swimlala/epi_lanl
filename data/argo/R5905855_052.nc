CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:19:39Z creation;2022-06-04T19:19:39Z conversion to V3.1      
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
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
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
_FillValue                 �  �P   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604191939  20220610151508  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               4A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�(P�?�1   @�(�
��@-ڟ�vȴ�c`�t�j1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @9��@�  @�33A   A   AA��A`  A�  A�  A�33A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBh  Bo��BxffB�  B���B���B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB���B�  B�  B���B�  B�  B�  B�  C   C  C  C  C  C
L�C��C�fC  C  C  C  C  C  C�fC  C   C"  C$  C&  C(  C*  C,  C.  C0�C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd�Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�<�D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@4z�@z�H@���@�p�A�RA@Q�A^�RA~�RA�\)A��\A�\)A�\)A�\)A�\)A�\)A�\)B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B`zBg�BoG�BxzB�B�p�B���B��
B��
B��
B��
B��
B��
B��
B�
=B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B�=pBߣ�B��
B��
B��B��
B��
B��
B��
B��
C�C�C�C�C
8RC�RC��C�C�C�C�C�C�C��C�C�C!�C#�C%�C'�C)�C+�C-�C0C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�CdCe�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D	z�D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D#��D$z�D$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtz�Dt��Duz�Du��Dvz�Dv��Dwz�Dw��Dxz�Dx��Dyz�Dy��Dzz�Dz��D{z�D{��D|z�D|��D}z�D}��D~z�D~��Dz�D��D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD½qD��qD�=qD�}qDýqD��qD�=qD�}qDĽqD��qD�=qD�}qDŽqD��qD�=qD�}qDƽqD��qD�=qD�}qDǽqD��qD�=qD�}qDȽqD��qD�=qD�}qDɽqD��qD�=qD�}qDʽqD��qD�=qD�}qD˽qD��qD�=qD�}qD̽qD��qD�=qD�}qDͽqD��qD�=qD�}qDνqD��qD�=qD�}qDϽqD��qD�=qD�}qDнqD��qD�=qD�}qDѽqD��qD�=qD�}qDҽqD��qD�=qD�}qDӽqD��qD�=qD�}qDԽqD��qD�=qD�}qDսqD��qD�=qD�}qDֽqD��qD�=qD�}qD׽qD��qD�=qD�}qDؽqD��qD�=qD�}qDٽqD��qD�=qD�}qDڽqD��qD�=qD�}qD۽qD��qD�=qD�}qDܽqD��qD�:>D�}qDݽqD��qD�=qD�}qD޽qD��qD�=qD�}qD߽qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A��3A���A���A��mA���A��}A��A��A�ɺA���A���A�یA���A��]A���A��dA��?A��dA�ޞA���A��gA�՛A���A���A��A���AӤ�A�ncA�h�A΍A��A�<�AǛ�AíwA�A�j�A�;A��dA�YA���A��'A��}A���A�n�A��RA���A��#A� 4A�q�A�	�A�B�A���A�Y�A��A�~�A��%A�U�A��A���A���A��#A��bA��8A��A��;A�i�A�+6A��'A�~�A���A��xA��XA��A�dZA�+A���A�j�A�=�A�P�A���A���A�a�A��A�b�A�A|��Av�sAv\�Au��AszxAp��AnQ�Ak�UAg��Ac��Aa\)A_�rA^8�A]1AZ�KAV0UAP�xAL֡AJ�fAI��AF�:ACk�A@��A?
=A>��A=8�A;QA:/�A9�A9~(A8,=A6�dA5`�A4�A5�A4#�A2�A1_A/A-��A*��A(=A&�}A'FtA&�{A%"�A"y>AC�AD�A�A��A�As�A{A�uA�#A[WA=A�{AL0A��AA�'A�PA�QA��Aa|AMA��AC�A�hA�A�pA�A��A!�A5?A�A��A=A��A�A�YA��A��A�;Ag�A7LA:*AIRA	�A7A;dA^5A��AsA^�A�mA&�A��A��AVAK^A \A�&A��A�AL0A�KA�TA�A�3A�RA��Ap�A_�A�A��A*�A��AQAOvA�"AOA ��A n/A *�@�n�@�G@�1�@�.�@�L0@�?@�?@��U@���@���@@��@��@�)�@���@��@��@�2a@�$@�� @鸻@�n/@���@�p;@�Q�@��@�-w@�r�@��@�W�@���@�Z�@�o @�'@��K@��@�K^@�~@�@@�.I@��@��@��@���@�q@�@���@��3@��p@�[�@��)@��@�YK@���@ۆ�@�x�@���@�Q�@�@�@��
@��@�M@�	@�e�@���@��@�J�@�K�@ف�@��@�{@��,@��m@�(�@�?@��@ޔF@�b�@�p�@�#:@��B@�ԕ@��@��[@�'�@���@��@�e,@��@־@�%�@�j@�"�@��@�q@��@�8�@�,=@��j@��@�g�@�#�@�+k@�^�@Κ�@���@�/@̼j@�u@��m@�C�@�ff@��@ɫ�@�x@��@�z@ǒ:@�,�@�҉@�l"@�n/@��M@Ā�@��@Ì~@�	l@��]@��p@@�($@���@�	l@��-@�;@�M�@��#@���@�S@���@�N�@��@��d@���@�w2@�J�@��2@�|�@�� @�G�@�r�@��@��#@��{@��@�h
@�4@���@�X@�$t@��y@���@�+k@��w@���@�O@���@���@�[�@�#�@��@��A@��6@��t@�y�@�L�@��@��@���@�e�@�ݘ@�iD@�-w@��s@��A@�6@��@�j�@�6z@���@���@�u�@�M@��H@�H�@��`@�Xy@�{@���@���@�S�@��@��@���@�w�@�A�@���@�j�@��@��,@���@�Ft@���@��7@�n/@�<6@���@���@���@�/�@��O@�z�@�/�@��@���@�]�@�@��6@�;�@�b@��m@���@���@��3@��@@�G�@��`@��I@�R�@��@���@��7@�\�@���@�V�@�	�@���@�2a@�w�@�@�@�B[@�-�@��@�4@�4@�1@��g@�33@��`@���@��u@�Z@�/�@�7@���@���@�X@�6z@��'@�YK@�/�@�($@�%�@���@���@��-@�j�@��@�ߤ@���@�q@�M@���@���@�x�@�4@��c@��m@���@�^5@�'R@���@��f@�rG@�^�@�IR@�33@�!-@�V@���@���@���@�U2@���@�|@�Dg@�@���@���@�L0@�;�@�~@���@��@�f�@�)_@��@�S@��|@�r�@��@�  @��W@���@���@�j�@�;d@���@�� @�Ov@��@��@���@�K�@��]@�YK@��@��d@��V@�*0@��@���@��)@�oi@��@��@��Q@�� @��@��^@���@�a@�#�@��@��@�_@�@��Q@�s@�+@��U@�d�@�Q�@�B[@�+k@��@ƨ@�:@Mj@!-@~��@~��@}��@}��@}o @|�@|�@{�W@{�@{X�@{�@z��@zR�@y�=@x��@x��@x/�@w��@wMj@w1�@v҉@vW�@u�@ue,@u8�@t�@t��@tZ@s��@r�@r:*@q�#@qzx@p�[@p�D@p[�@p$@o��@o33@n��@n͟@n�@nGE@n�@nJ@m��@m8�@lK^@ky�@k,�@j��@jz@jB[@i��@iq@h�@h �@g��@g�g@g�V@g~�@g!-@fTa@e�S@e^�@eIR@eF@e?}@e�@d�`@d��@dw�@d7�@d�@d  @c��@b�M@b@�@b3�@b)�@a�D@a�t@a��@a-w@`��@`U2@`G@_�4@_�@^�R@^W�@]�@]��@]��@];@\h�@[��@[x@[P�@[(@Z�M@Zn�@Y��@Ym]@X�4@X9X@Xb@W�V@WX�@V�M@V��@Uϫ@Ux�@UB�@T��@T�@T~(@S��@SA�@R͟@R��@R=q@Ru@Q�z@Q�@Q�M@Q\�@Q2a@P��@P�@O�@N{�@M�>@M��@MB�@M%@L�.@Lm�@LFt@K�
@K��@J�@J�@J6�@I��@I�@H��@H�[@H�@HXy@G��@G�	@GH�@G
=@Fں@F�\@F)�@E��@D�K@D��@D�@D:�@C�@C��@Cs@C6z@B��@B�\@B-@A��@A<6@@�`@@~(@@*�@@�@?�W@?˒@?{J@>�m@>C�@>	@=��@=X@=A @=�@<��@<�_@<l"@<H@<%�@;��@;�@;s@;o@:�]@:GE@9�@9��@9Q�@9@8�@8��@8�D@8u�@8Ft@7��@7>�@6��@60U@5�T@5�H@5�~@5IR@5&�@5@@4��@4e�@3�]@3o�@3/�@3�@2��@2�@2��@2i�@2+k@2	@1�9@1c�@0֡@0h�@0K^@0 �@/��@/��@/��@/P�@.��@.��@.�r@.z@-�@-ԕ@-��@-zx@-=�@,�5@,�O@,Xy@,<�@,!@+�]@+�W@+�g@+{J@+>�@+�@+�@+�@+@*��@*ff@*:*@)�>@)��@)J�@)*0@)�@(�p@(�4@(tT@(C-@(@'�@'�w@'��@'_p@'.I@'�@&�@&͟@&}V@&H�@%��@%��@%p�@%T�@%�@$�@$�@$l"@$D�@$�@#�@#��@#�@#>�@#�@#�@"��@"�@"�]@"s�@"_�@"c @"=q@!�d@!��@!T�@!�@ �@ ��@ �u@ ��@ z�@ j@ *�@��@�@@��@�	@s@X�@!-@�]@��@$�@�T@�N@�7@2a@��@��@��@�O@u�@�@�@��@C�@�2@�}@��@E�@��@:�@2a@�)@��@u�@l"@Ft@�&@�@@dZ@;d@@�@��@�!@h
@GE@�@��@��@^�@0�@�@��@�@��@��@4n@��@�k@\)@+@��@��@@�@J@�z@��@o @:�@�@�4@��@q@Q�@6@��@�F@�V@~�@$t@��@�2@�m@��@s�@h
@YK@;�@��@�3@}�@7L@+@�	@��@��@z�@N�@>B@/�@M@�Q@��@��@s@\)@;d@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A��3A���A���A��mA���A��}A��A��A�ɺA���A���A�یA���A��]A���A��dA��?A��dA�ޞA���A��gA�՛A���A���A��A���AӤ�A�ncA�h�A΍A��A�<�AǛ�AíwA�A�j�A�;A��dA�YA���A��'A��}A���A�n�A��RA���A��#A� 4A�q�A�	�A�B�A���A�Y�A��A�~�A��%A�U�A��A���A���A��#A��bA��8A��A��;A�i�A�+6A��'A�~�A���A��xA��XA��A�dZA�+A���A�j�A�=�A�P�A���A���A�a�A��A�b�A�A|��Av�sAv\�Au��AszxAp��AnQ�Ak�UAg��Ac��Aa\)A_�rA^8�A]1AZ�KAV0UAP�xAL֡AJ�fAI��AF�:ACk�A@��A?
=A>��A=8�A;QA:/�A9�A9~(A8,=A6�dA5`�A4�A5�A4#�A2�A1_A/A-��A*��A(=A&�}A'FtA&�{A%"�A"y>AC�AD�A�A��A�As�A{A�uA�#A[WA=A�{AL0A��AA�'A�PA�QA��Aa|AMA��AC�A�hA�A�pA�A��A!�A5?A�A��A=A��A�A�YA��A��A�;Ag�A7LA:*AIRA	�A7A;dA^5A��AsA^�A�mA&�A��A��AVAK^A \A�&A��A�AL0A�KA�TA�A�3A�RA��Ap�A_�A�A��A*�A��AQAOvA�"AOA ��A n/A *�@�n�@�G@�1�@�.�@�L0@�?@�?@��U@���@���@@��@��@�)�@���@��@��@�2a@�$@�� @鸻@�n/@���@�p;@�Q�@��@�-w@�r�@��@�W�@���@�Z�@�o @�'@��K@��@�K^@�~@�@@�.I@��@��@��@���@�q@�@���@��3@��p@�[�@��)@��@�YK@���@ۆ�@�x�@���@�Q�@�@�@��
@��@�M@�	@�e�@���@��@�J�@�K�@ف�@��@�{@��,@��m@�(�@�?@��@ޔF@�b�@�p�@�#:@��B@�ԕ@��@��[@�'�@���@��@�e,@��@־@�%�@�j@�"�@��@�q@��@�8�@�,=@��j@��@�g�@�#�@�+k@�^�@Κ�@���@�/@̼j@�u@��m@�C�@�ff@��@ɫ�@�x@��@�z@ǒ:@�,�@�҉@�l"@�n/@��M@Ā�@��@Ì~@�	l@��]@��p@@�($@���@�	l@��-@�;@�M�@��#@���@�S@���@�N�@��@��d@���@�w2@�J�@��2@�|�@�� @�G�@�r�@��@��#@��{@��@�h
@�4@���@�X@�$t@��y@���@�+k@��w@���@�O@���@���@�[�@�#�@��@��A@��6@��t@�y�@�L�@��@��@���@�e�@�ݘ@�iD@�-w@��s@��A@�6@��@�j�@�6z@���@���@�u�@�M@��H@�H�@��`@�Xy@�{@���@���@�S�@��@��@���@�w�@�A�@���@�j�@��@��,@���@�Ft@���@��7@�n/@�<6@���@���@���@�/�@��O@�z�@�/�@��@���@�]�@�@��6@�;�@�b@��m@���@���@��3@��@@�G�@��`@��I@�R�@��@���@��7@�\�@���@�V�@�	�@���@�2a@�w�@�@�@�B[@�-�@��@�4@�4@�1@��g@�33@��`@���@��u@�Z@�/�@�7@���@���@�X@�6z@��'@�YK@�/�@�($@�%�@���@���@��-@�j�@��@�ߤ@���@�q@�M@���@���@�x�@�4@��c@��m@���@�^5@�'R@���@��f@�rG@�^�@�IR@�33@�!-@�V@���@���@���@�U2@���@�|@�Dg@�@���@���@�L0@�;�@�~@���@��@�f�@�)_@��@�S@��|@�r�@��@�  @��W@���@���@�j�@�;d@���@�� @�Ov@��@��@���@�K�@��]@�YK@��@��d@��V@�*0@��@���@��)@�oi@��@��@��Q@�� @��@��^@���@�a@�#�@��@��@�_@�@��Q@�s@�+@��U@�d�@�Q�@�B[@�+k@��@ƨ@�:@Mj@!-@~��@~��@}��@}��@}o @|�@|�@{�W@{�@{X�@{�@z��@zR�@y�=@x��@x��@x/�@w��@wMj@w1�@v҉@vW�@u�@ue,@u8�@t�@t��@tZ@s��@r�@r:*@q�#@qzx@p�[@p�D@p[�@p$@o��@o33@n��@n͟@n�@nGE@n�@nJ@m��@m8�@lK^@ky�@k,�@j��@jz@jB[@i��@iq@h�@h �@g��@g�g@g�V@g~�@g!-@fTa@e�S@e^�@eIR@eF@e?}@e�@d�`@d��@dw�@d7�@d�@d  @c��@b�M@b@�@b3�@b)�@a�D@a�t@a��@a-w@`��@`U2@`G@_�4@_�@^�R@^W�@]�@]��@]��@];@\h�@[��@[x@[P�@[(@Z�M@Zn�@Y��@Ym]@X�4@X9X@Xb@W�V@WX�@V�M@V��@Uϫ@Ux�@UB�@T��@T�@T~(@S��@SA�@R͟@R��@R=q@Ru@Q�z@Q�@Q�M@Q\�@Q2a@P��@P�@O�@N{�@M�>@M��@MB�@M%@L�.@Lm�@LFt@K�
@K��@J�@J�@J6�@I��@I�@H��@H�[@H�@HXy@G��@G�	@GH�@G
=@Fں@F�\@F)�@E��@D�K@D��@D�@D:�@C�@C��@Cs@C6z@B��@B�\@B-@A��@A<6@@�`@@~(@@*�@@�@?�W@?˒@?{J@>�m@>C�@>	@=��@=X@=A @=�@<��@<�_@<l"@<H@<%�@;��@;�@;s@;o@:�]@:GE@9�@9��@9Q�@9@8�@8��@8�D@8u�@8Ft@7��@7>�@6��@60U@5�T@5�H@5�~@5IR@5&�@5@@4��@4e�@3�]@3o�@3/�@3�@2��@2�@2��@2i�@2+k@2	@1�9@1c�@0֡@0h�@0K^@0 �@/��@/��@/��@/P�@.��@.��@.�r@.z@-�@-ԕ@-��@-zx@-=�@,�5@,�O@,Xy@,<�@,!@+�]@+�W@+�g@+{J@+>�@+�@+�@+�@+@*��@*ff@*:*@)�>@)��@)J�@)*0@)�@(�p@(�4@(tT@(C-@(@'�@'�w@'��@'_p@'.I@'�@&�@&͟@&}V@&H�@%��@%��@%p�@%T�@%�@$�@$�@$l"@$D�@$�@#�@#��@#�@#>�@#�@#�@"��@"�@"�]@"s�@"_�@"c @"=q@!�d@!��@!T�@!�@ �@ ��@ �u@ ��@ z�@ j@ *�@��@�@@��@�	@s@X�@!-@�]@��@$�@�T@�N@�7@2a@��@��@��@�O@u�@�@�@��@C�@�2@�}@��@E�@��@:�@2a@�)@��@u�@l"@Ft@�&@�@@dZ@;d@@�@��@�!@h
@GE@�@��@��@^�@0�@�@��@�@��@��@4n@��@�k@\)@+@��@��@@�@J@�z@��@o @:�@�@�4@��@q@Q�@6@��@�F@�V@~�@$t@��@�2@�m@��@s�@h
@YK@;�@��@�3@}�@7L@+@�	@��@��@z�@N�@>B@/�@M@�Q@��@��@s@\)@;d@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	SuB	S�B	SuB	S�B	S�B	S�B	S�B	S�B	S�B	S�B	SuB	S�B	S�B	S�B	S�B	S�B	S�B	SuB	S[B	S[B	S@B	R�B	R�B	SB	S&B	R B	RTB	P�B	NpB	9�B	 'B	�B	�B	,�B	~�B	�UB	�fB	��B	��B
	RB
'�B
'mB
2�B
=�B
i�B
�xB
�B
̘B
�SB
�LB
��B
�B�B./BV�B]dBe,B�<B�B� BǔB�B��B��B��B�B�FB�6Be�BJ�B0UB�B OB
�;B
�yB
��B
�B
�RB
��B
��B
�RB
s�B
!-B
dB	��B	��B	�=B	ʌB	�zB	�'B	��B	�1B	��B	poB	R�B	2|B	~B		B	�B	sB	�B	�B�cB�B�XB��B��B��B�SB��B޸B��B��B	�B	B	�B	�B	�B	�B	(XB	?}B	5�B	'RB	%�B	�B	B	1B�?B��B	pB	1�B	1[B	�B��B�BԯB�aBՁBؓB�KB��B	tB	 OB	�B	�B	[�B	w�B	�B	�B	~�B	{�B	w�B	v�B	�B	��B	��B	�EB	��B	�CB	�tB	�B	��B	�5B	��B	��B	�zB	��B	��B	�YB	�B	�KB	�:B	��B	��B	��B	��B	�3B	�B	�B	��B	�DB	��B	��B	��B	��B	��B	ڠB	��B	��B	��B	ݲB	��B	�CB	�KB	�#B	�B	�VB	��B	�NB	�TB	�B	�tB	�fB	��B	�2B	�B	��B	�yB	�nB	��B	�nB	�hB	��B	�eB	�B	�B	ѝB	��B	� B	�B	��B	��B	�CB	� B	�IB	��B	��B	��B	��B	��B	��B	��B	��B	�WB	�kB	��B	�B	��B	��B	��B	�nB	�nB	�2B	��B	�tB	��B	��B	��B	��B	��B	�}B	�B	��B	��B	�XB	��B	��B	�mB	�LB	�zB	��B	�@B	��B	�B	�B	�/B	��B	��B	��B	�eB	��B	��B	�
B	��B	��B	��B	��B	�;B	�NB	��B	� B	��B	�ZB	��B	�*B	��B	��B	��B	�"B	��B	�_B	�HB	ǮB	�-B	� B	�"B	�2B	�eB	��B	�bB	�hB	�B	��B	�NB	�B	�'B	��B	��B	�hB	�nB	��B	�B	�B	�B	�2B	��B	�B	�nB	�nB	��B	�:B	�B	��B	�B	�TB	��B	��B	�`B	�B	�2B	��B	�$B	�$B	�$B	��B	�B	�B	�B	�B	�mB	�>B	�B	�B	�B	�B	�yB	�0B	�=B	�WB	��B	�B	�CB	�/B	� B	�B	� B	�OB	�OB	�B	�B	�;B	��B	�'B	�[B	�B	�B	��B	��B	�MB	�B	��B	��B	��B	�nB	�nB	��B	�?B	��B	�tB	�+B	�zB	��B	��B	�B	��B	�RB	��B	��B	��B	��B	�>B	�XB	�$B	�>B	�B	��B	�B	��B	�jB	�<B	�B	��B	��B	��B	��B	��B
  B
 B
 �B
 �B
�B
oB
�B
uB
�B
�B
�B
-B
-B
{B
�B
3B
�B
�B
�B
B
�B
�B
�B
�B
%B
?B
EB
�B
B
1B
�B
�B
	B
	7B
	�B

�B

�B
B
^B
�B
�B
NB
�B
�B
&B
�B
�B
�B
�B
�B
�B
mB
�B
,B
B
�B
�B
�B
,B
�B
B
2B
B
B
B
B
FB
aB
�B
gB
�B
�B
�B
�B
�B
gB
�B
B
mB
�B
�B
�B
�B
�B
�B
?B
?B
?B
�B
+B
_B
�B
�B
�B
1B
1B
B
KB
B
�B
�B
=B
qB
�B
B
B
CB
B
)B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
5B
OB
�B
�B
B
B
�B
�B
 B
 B
 'B
 vB
 �B
 �B
 �B
!|B
"hB
"�B
"�B
#B
$�B
$�B
%FB
%�B
&2B
&fB
&�B
'�B
'�B
(
B
'�B
(�B
)�B
*eB
*KB
*KB
*KB
*eB
+B
+B
+�B
+�B
,"B
-]B
-]B
-wB
/ B
0�B
1�B
1�B
2-B
2�B
2�B
2�B
3B
3B
2�B
2�B
2�B
2�B
3B
33B
3MB
3�B
3�B
4B
4B
4B
4nB
4�B
5ZB
6B
5�B
5�B
72B
7�B
7�B
7�B
8RB
8�B
8�B
8�B
8�B
9XB
9XB
9rB
9�B
:xB
:�B
:�B
:�B
;B
;�B
;�B
;�B
<B
<B
<B
<B
<B
<6B
<6B
<B
<PB
<jB
=<B
=�B
>B
>BB
>�B
>�B
>�B
?}B
?�B
@B
@B
@OB
@�B
@�B
@�B
A�B
B'B
BAB
B[B
B[B
BAB
B[B
B�B
B�B
B�B
CB
C-B
CB
B�B
D3B
D�B
DgB
D�B
D�B
D�B
EB
EmB
EmB
E�B
FB
FYB
F�B
F�B
G_B
G�B
G�B
G�B
G�B
HB
H�B
H�B
H�B
H�B
IB
I7B
IlB
I�B
J	B
JrB
J�B
J�B
J�B
KB
J�B
KB
K)B
K)B
KxB
KxB
KxB
LB
L~B
MB
MPB
M�B
M�B
M�B
M�B
M�B
N"B
M�B
N�B
N�B
OvB
O�B
PB
P}B
P�B
P�B
Q B
QB
QB
QhB
Q�B
R:B
R:B
R�B
R�B
SB
S@B
S[B
S�B
S�B
TB
S�B
T,B
TB
TB
T,B
TFB
T�B
UgB
UgB
UMB
U�B
U�B
U�B
VB
VB
VSB
V�B
V�B
W$B
WsB
WsB
W�B
W�B
W�B
XB
XB
XB
X�B
X�B
X�B
Y�B
Y�B
Y�B
Y�B
Z7B
ZkB
Z�B
Z�B
Z�B
Z�B
Z�B
[#B
[qB
[WB
[�B
\B
\)B
\]B
\xB
\�B
\�B
\�B
\�B
\�B
]IB
]�B
]�B
^jB
^jB
^�B
^�B
^�B
^�B
_B
_B
_VB
_�B
`'B
`BB
`BB
`vB
`�B
`�B
`�B
`�B
`�B
`�B
abB
a�B
b4B
bB
bNB
bhB
bhB
b�B
b�B
cB
cTB
cTB
c B
c�B
c�B
c�B
c�B
dB
d@B
dtB
d�B
d�B
d�B
d�B
d�B
d�B
e,B
ezB
e`B
e`B
ezB
ezB
e`B
fB
e�B
fLB
f�B
f�B
f�B
f�B
gB
g8B
gRB
gmB
g�B
g�B
g�B
g�B
h$B
h$B
hXB
h>B
hsB
h�B
h�B
iB
iB
i_B
i_B
i�B
i�B
i�B
jB
j0B
jKB
j�B
jKB
j�B
kB
kB
kB
kB
k6B
kB
k�B
k�B
k�B
k�B
l=B
lWB
l�B
l�B
l�B
mB
mCB
mCB
mCB
m)B
mwB
m�B
m�B
m�B
nB
n�B
n�B
n�B
n�B
o5B
oB
oiB
oOB
o�B
o�B
p;B
p;B
p;B
poB
p�B
p�B
q[B
q[B
q�B
r-B
r-B
rB
rB
r�B
r�B
r�B
sMB
shB
s�B
s�B
s�B
tB
tTB
t�B
t�B
uB
uB
u?B
utB
u�B
u�B
u�B
u�B
vB
vzB
v�B
v�B
wB
wB
w2B
v�B
wfB
w�B
w�B
xB
xB
xRB
x8B
x�B
x�B
y$B
yXB
y>B
y�B
y�B
zDB
z*B
z^B
zxB
z^B
z�B
z�B
{0B
z�B
{�B
{�B
{�B
|B
|B
|PB
|6B
|6B
|PB
|�B
|�B
|�B
}"B
}"B
}qB
}qB
}�B
}�B
~B
~(B
~BB
~(B
~�B
~�B
~�B
.B
.B
B
H111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	SuB	S�B	SuB	S�B	S�B	S�B	S�B	S�B	S�B	S�B	SuB	S�B	S�B	S�B	S�B	S�B	S�B	SuB	S[B	S[B	S@B	R�B	R�B	SB	S&B	R B	RTB	P�B	NpB	9�B	 'B	�B	�B	,�B	~�B	�UB	�fB	��B	��B
	RB
'�B
'mB
2�B
=�B
i�B
�xB
�B
̘B
�SB
�LB
��B
�B�B./BV�B]dBe,B�<B�B� BǔB�B��B��B��B�B�FB�6Be�BJ�B0UB�B OB
�;B
�yB
��B
�B
�RB
��B
��B
�RB
s�B
!-B
dB	��B	��B	�=B	ʌB	�zB	�'B	��B	�1B	��B	poB	R�B	2|B	~B		B	�B	sB	�B	�B�cB�B�XB��B��B��B�SB��B޸B��B��B	�B	B	�B	�B	�B	�B	(XB	?}B	5�B	'RB	%�B	�B	B	1B�?B��B	pB	1�B	1[B	�B��B�BԯB�aBՁBؓB�KB��B	tB	 OB	�B	�B	[�B	w�B	�B	�B	~�B	{�B	w�B	v�B	�B	��B	��B	�EB	��B	�CB	�tB	�B	��B	�5B	��B	��B	�zB	��B	��B	�YB	�B	�KB	�:B	��B	��B	��B	��B	�3B	�B	�B	��B	�DB	��B	��B	��B	��B	��B	ڠB	��B	��B	��B	ݲB	��B	�CB	�KB	�#B	�B	�VB	��B	�NB	�TB	�B	�tB	�fB	��B	�2B	�B	��B	�yB	�nB	��B	�nB	�hB	��B	�eB	�B	�B	ѝB	��B	� B	�B	��B	��B	�CB	� B	�IB	��B	��B	��B	��B	��B	��B	��B	��B	�WB	�kB	��B	�B	��B	��B	��B	�nB	�nB	�2B	��B	�tB	��B	��B	��B	��B	��B	�}B	�B	��B	��B	�XB	��B	��B	�mB	�LB	�zB	��B	�@B	��B	�B	�B	�/B	��B	��B	��B	�eB	��B	��B	�
B	��B	��B	��B	��B	�;B	�NB	��B	� B	��B	�ZB	��B	�*B	��B	��B	��B	�"B	��B	�_B	�HB	ǮB	�-B	� B	�"B	�2B	�eB	��B	�bB	�hB	�B	��B	�NB	�B	�'B	��B	��B	�hB	�nB	��B	�B	�B	�B	�2B	��B	�B	�nB	�nB	��B	�:B	�B	��B	�B	�TB	��B	��B	�`B	�B	�2B	��B	�$B	�$B	�$B	��B	�B	�B	�B	�B	�mB	�>B	�B	�B	�B	�B	�yB	�0B	�=B	�WB	��B	�B	�CB	�/B	� B	�B	� B	�OB	�OB	�B	�B	�;B	��B	�'B	�[B	�B	�B	��B	��B	�MB	�B	��B	��B	��B	�nB	�nB	��B	�?B	��B	�tB	�+B	�zB	��B	��B	�B	��B	�RB	��B	��B	��B	��B	�>B	�XB	�$B	�>B	�B	��B	�B	��B	�jB	�<B	�B	��B	��B	��B	��B	��B
  B
 B
 �B
 �B
�B
oB
�B
uB
�B
�B
�B
-B
-B
{B
�B
3B
�B
�B
�B
B
�B
�B
�B
�B
%B
?B
EB
�B
B
1B
�B
�B
	B
	7B
	�B

�B

�B
B
^B
�B
�B
NB
�B
�B
&B
�B
�B
�B
�B
�B
�B
mB
�B
,B
B
�B
�B
�B
,B
�B
B
2B
B
B
B
B
FB
aB
�B
gB
�B
�B
�B
�B
�B
gB
�B
B
mB
�B
�B
�B
�B
�B
�B
?B
?B
?B
�B
+B
_B
�B
�B
�B
1B
1B
B
KB
B
�B
�B
=B
qB
�B
B
B
CB
B
)B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
5B
OB
�B
�B
B
B
�B
�B
 B
 B
 'B
 vB
 �B
 �B
 �B
!|B
"hB
"�B
"�B
#B
$�B
$�B
%FB
%�B
&2B
&fB
&�B
'�B
'�B
(
B
'�B
(�B
)�B
*eB
*KB
*KB
*KB
*eB
+B
+B
+�B
+�B
,"B
-]B
-]B
-wB
/ B
0�B
1�B
1�B
2-B
2�B
2�B
2�B
3B
3B
2�B
2�B
2�B
2�B
3B
33B
3MB
3�B
3�B
4B
4B
4B
4nB
4�B
5ZB
6B
5�B
5�B
72B
7�B
7�B
7�B
8RB
8�B
8�B
8�B
8�B
9XB
9XB
9rB
9�B
:xB
:�B
:�B
:�B
;B
;�B
;�B
;�B
<B
<B
<B
<B
<B
<6B
<6B
<B
<PB
<jB
=<B
=�B
>B
>BB
>�B
>�B
>�B
?}B
?�B
@B
@B
@OB
@�B
@�B
@�B
A�B
B'B
BAB
B[B
B[B
BAB
B[B
B�B
B�B
B�B
CB
C-B
CB
B�B
D3B
D�B
DgB
D�B
D�B
D�B
EB
EmB
EmB
E�B
FB
FYB
F�B
F�B
G_B
G�B
G�B
G�B
G�B
HB
H�B
H�B
H�B
H�B
IB
I7B
IlB
I�B
J	B
JrB
J�B
J�B
J�B
KB
J�B
KB
K)B
K)B
KxB
KxB
KxB
LB
L~B
MB
MPB
M�B
M�B
M�B
M�B
M�B
N"B
M�B
N�B
N�B
OvB
O�B
PB
P}B
P�B
P�B
Q B
QB
QB
QhB
Q�B
R:B
R:B
R�B
R�B
SB
S@B
S[B
S�B
S�B
TB
S�B
T,B
TB
TB
T,B
TFB
T�B
UgB
UgB
UMB
U�B
U�B
U�B
VB
VB
VSB
V�B
V�B
W$B
WsB
WsB
W�B
W�B
W�B
XB
XB
XB
X�B
X�B
X�B
Y�B
Y�B
Y�B
Y�B
Z7B
ZkB
Z�B
Z�B
Z�B
Z�B
Z�B
[#B
[qB
[WB
[�B
\B
\)B
\]B
\xB
\�B
\�B
\�B
\�B
\�B
]IB
]�B
]�B
^jB
^jB
^�B
^�B
^�B
^�B
_B
_B
_VB
_�B
`'B
`BB
`BB
`vB
`�B
`�B
`�B
`�B
`�B
`�B
abB
a�B
b4B
bB
bNB
bhB
bhB
b�B
b�B
cB
cTB
cTB
c B
c�B
c�B
c�B
c�B
dB
d@B
dtB
d�B
d�B
d�B
d�B
d�B
d�B
e,B
ezB
e`B
e`B
ezB
ezB
e`B
fB
e�B
fLB
f�B
f�B
f�B
f�B
gB
g8B
gRB
gmB
g�B
g�B
g�B
g�B
h$B
h$B
hXB
h>B
hsB
h�B
h�B
iB
iB
i_B
i_B
i�B
i�B
i�B
jB
j0B
jKB
j�B
jKB
j�B
kB
kB
kB
kB
k6B
kB
k�B
k�B
k�B
k�B
l=B
lWB
l�B
l�B
l�B
mB
mCB
mCB
mCB
m)B
mwB
m�B
m�B
m�B
nB
n�B
n�B
n�B
n�B
o5B
oB
oiB
oOB
o�B
o�B
p;B
p;B
p;B
poB
p�B
p�B
q[B
q[B
q�B
r-B
r-B
rB
rB
r�B
r�B
r�B
sMB
shB
s�B
s�B
s�B
tB
tTB
t�B
t�B
uB
uB
u?B
utB
u�B
u�B
u�B
u�B
vB
vzB
v�B
v�B
wB
wB
w2B
v�B
wfB
w�B
w�B
xB
xB
xRB
x8B
x�B
x�B
y$B
yXB
y>B
y�B
y�B
zDB
z*B
z^B
zxB
z^B
z�B
z�B
{0B
z�B
{�B
{�B
{�B
|B
|B
|PB
|6B
|6B
|PB
|�B
|�B
|�B
}"B
}"B
}qB
}qB
}�B
}�B
~B
~(B
~BB
~(B
~�B
~�B
~�B
.B
.B
B
H111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105238  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604191939  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604191939  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604191939                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605041947  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605041947  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151508                      G�O�G�O�G�O�                