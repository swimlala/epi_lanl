CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-07-20T15:42:18Z creation;2022-07-20T15:42:19Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220720154218  20220720155828  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               }A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�ߞ�r�b1   @�ߟU��	@0������c���R1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @���A   A   A@  A^ffA�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8ffB@ffBHffBP  BX  B_33Bh  Br  Bw33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�ffB���C  C�fC�fC  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8�C:�C<  C>  C@  CB  CC�fCF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch33Ci�fCl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD�fDfD�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DUfDU�fDV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�C3Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@z�@z�H@�=q@�p�A�RA>�RA]�A~�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B�B�B�B'�B/�B8zB@zBHzBO�BW�B^�GBg�Bq�Bv�GB�B��
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
B�=pB���B��
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
B��
B��
B��
B��
B�
=B�=pB���C�C��C��C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C8C:C;�C=�C?�CA�CC��CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Ch�Ci��Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D	z�D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�DGD�GDGD�GD��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D#��D$z�D$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DUGDU�GDU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtz�Dt��Duz�Du��Dvz�Dv��Dwz�Dw��Dxz�Dx��Dyz�Dy��Dzz�Dz��D{z�D{��D|z�D|��D}z�D}��D~z�D~��Dz�D��D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�@�D�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD���D��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD½qD��qD�=qD�}qDýqD��qD�=qD�}qDĽqD��qD�@�D�}qDŽqD��qD�=qD�}qDƽqD��qD�=qD�}qDǽqD��qD�=qD�}qDȽqD��qD�=qD�}qDɽqD��qD�=qD�}qDʽqD��qD�=qD�}qD˽qD��qD�=qD�}qD̽qD��qD�=qD�}qDͽqD��qD�=qD�}qDνqD��qD�=qD�}qDϽqD��qD�=qD�}qDнqD��qD�=qD�}qDѽqD��qD�=qD�}qDҽqD��qD�=qD�}qDӽqD��qD�=qD�}qDԽqD��qD�=qD�}qDսqD��qD�=qD�}qDֽqD��qD�=qD�}qD׽qD��qD�=qD�}qDؽqD��qD�=qD�}qDٽqD��qD�=qD�}qDڽqD��qD�=qD�}qD۽qD��qD�=qD�}qDܽqD��qD�=qD�}qDݽqD��qD�=qD�}qD޽qD��qD�=qD�}qD߽qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�z>D��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�OA�ZQA�YA�U�A�U�A�Y�A�]�A�^5A�`�A�bA�bNA�dZA�e�A�d�A�g8A�gA�bNA�c A�c A�c�A�b�A�`�A�_;A�O�A�NA�L�A�B[A��A�&LAǼ�A�m)A�:�A�ɺA�?}Aö�A��A��A�q�A��>A�F?A�g�A�:�A�*�A�F�A�
�A�?�A�g�A�O�A�ҽA�w�A�A�h>A�1�A��vA���A���A�/�A���A�Z�A��A�� A��gA���A��NA�V�A���A��A�j�A�{A���A�+A���A�S�A�$tA��A��A�4�A�y�A�k�A��wA��'A���A�@�A�7�A�1�A���A��`A���A�!�A���A�m�A��&A�?A��OA�2aA�/�A�j�A}�XAy��Au��Asg8ArQ�Aq
�Am��Ak%FAf;dAc��Aah�A\N�AU��AM�*AJe,AG�zAA��A>˒A=�MA8ZA4h
A3�xA2�A2N<A1��A1ffA0�6A/�nA-t�A,��A,<6A,a|A,��A,�A,�A+K^A+:�A+\�A+e�A+_A+&�A*\�A)�NA)@OA(�A(qA'�IA&�!A&|�A&J�A%|�A% \A$�A#�^A#f�A"��A!��A ��A T�A*0A[WA�A��Av`A+A�fA�_A��A+A�A��AK^A�0Al�A7LA�A��A��A�oA�A8�A^�A�|A�FAv�A�HAXyA�jA��AK^A�A^�AMAqA_A��A1'AA�A �A@A��AN<A��A��ArGAO�A,�AYA
�*A
}VA
?A
4A
�A	�RA	I�A�A��AVA��A|�A�	AzxA-A
=A�XAVA�@A_�A�A�xA;dA��AO�A��AA�A%AC-A�aA�VAZA ��A �@�-w@�q@���@�c@��.@��@�q�@�W?@��'@��o@���@�ԕ@� \@���@���@���@��	@�@�@��6@�A�@��@�6z@���@��@�4@�C@�@��@��@�^5@��3@�hs@�J�@�*0@��@��@�Z@�J@�P@�=�@��,@�.@�L0@駇@��@�v�@���@�t@�"@�k�@�f�@�O@�A @��@��j@��@�Ɇ@䟾@�tT@�
�@㋬@�F@��@�n�@�*�@�M@�C�@ߏ�@�8@���@�v�@��@�.I@ܿ�@�H@�@�J#@څ�@�?@���@��
@ّh@�#�@ا�@��T@��@�g8@ճ�@�y�@Հ4@���@���@Ӏ4@�8@���@Ҥ�@�D�@�e@��
@�C�@��p@�	@�@�z�@�{@���@�r�@ˎ�@�;d@�C@��@ɍP@�	@ǳ�@�
=@Ɩ�@���@Ł�@�T�@���@�C�@�c@�O�@�4@�@�#:@���@�Q�@�:�@� i@��B@���@��@�dZ@���@�_�@�0U@��@���@���@�RT@���@�-@��@��o@��a@��@���@�(@�ȴ@���@��U@���@��\@�(�@��@��0@�:�@��@��@�q@�7�@�~@�	@��@��-@��4@�%F@���@��Y@�ff@�\�@�C-@�2�@��@��@�J�@�M@���@��@��@@��S@�!-@��$@�q@�_@��q@�s@�E9@���@��)@���@��O@��@�p;@�H@��@���@�a�@��@��@���@��@�'R@��j@��{@�S&@�+�@��M@�Ɇ@���@�|�@�:�@�˒@�@O@��@���@�r�@��@���@�f�@�C�@��@��}@�{@��n@���@� �@��&@���@�k�@��@���@�w�@�^5@�/�@��w@��@@�x@�;d@��@�҉@���@�s�@��@��h@�7L@���@���@�Q�@��@��#@��F@��=@�S&@�֡@�4@��@���@��7@�;d@��@��@��@��|@���@�bN@�;�@�,=@��@�w2@�T�@�4@���@��@�d�@�6�@���@��@�H�@��y@��+@�S�@�<�@���@��6@�{J@�6z@� i@���@��X@���@���@�_@��{@�o@���@�Q�@�ƨ@���@�j@��@��@��\@�p;@�p;@�e�@�=q@�	@�ԕ@��@��@�$t@�ѷ@��}@�l"@�;�@�!�@��@�w2@�q@���@���@�V�@�  @��f@�+@��f@��H@��$@���@�YK@���@�|�@�e�@�!-@���@�H�@�J@��3@�F�@��9@�a|@�%�@��^@�Q�@�33@�@��f@���@��Y@�6�@�.�@��9@���@�o�@�Dg@��@��9@�*�@��d@���@��"@�J�@��@�z@�($@��@��z@�g�@��H@��@�YK@�{@��@�@@�V@\)@~�@~l�@~ �@}J�@|�v@|�@|S�@|b@{�
@{�$@z�y@zGE@y��@y�3@y�7@y+�@y&�@x�@x�@w��@wK�@wY@v��@vc @v1�@vGE@vE�@v#:@u�7@uDg@u@@t��@t:�@t*�@t�@s�@st�@sC@r��@rp;@rO@qϫ@q��@qO�@p��@pz�@p6@p@o�	@oE9@n�@n1�@mϫ@m��@m=�@lU2@k�a@k!-@j�@j�!@jOv@i��@i \@h�@h�u@hC-@g��@gZ�@f�m@f4@e��@e5�@d��@d4n@c��@c� @c�6@cP�@b�@bL0@a�^@`�$@_��@_�*@_�f@__p@_4�@^��@^��@^d�@^)�@]�)@]ԕ@]��@]@@\��@\"h@[ƨ@[n/@[C@[�@Z��@Z{@Y�d@Yc�@Y�@X�9@W�w@V�b@V{@Ua�@T?�@S��@S�q@S|�@R�"@R�@QY�@Q�@P�@P�@P�@P>B@O��@OiD@N��@N��@N�m@NR�@M�o@M�^@M�~@Mw2@M`B@MB�@M�@L�@L�`@L�[@L�@LA�@K�r@K�g@J��@J^5@IrG@Iq@I�@H�	@H�E@H�4@Hj@H<�@G�A@G��@G�:@G��@Ge�@GO@F�X@FJ�@E�.@E�@E�C@E[W@E%F@D�@D�j@D��@D�o@D_@D'R@C�r@Cy�@B�B@Bq�@B-@A�'@Aa�@AJ�@A:�@@�@@1@?�F@?��@?��@>�@>E�@>6�@>!�@>{@=�)@=��@=\�@=-w@<��@<�@<y>@;��@;1�@:i�@9�9@9��@97L@8w�@7�$@7��@7x@6�@6p;@6@�@60U@6�@5�@5J�@4�@4�@3�f@3iD@3/�@2�M@2�h@2��@2��@21�@2�@1��@1��@1`B@1%@0��@0�@/�6@/��@/]�@/ i@.��@.q�@.O@.u@-�@,�@,e�@,1'@,G@+�q@+n/@+F�@+ i@*��@*��@*q�@*5?@*#:@)�.@)�Z@)�3@)�~@)�@(�$@(��@(!@'��@'>�@&�y@&�@&��@&�r@&s�@&�@%�@%��@%|@%5�@$�@$��@$g8@$>B@$%�@$�@#�a@#l�@#�@"ff@"GE@"5?@!�@!�n@!2a@!;@ �`@ ��@ ��@ ��@ ��@ |�@ oi@ [�@ 4n@   @�@�@��@U�@@�m@��@p;@YK@4@�N@�S@rG@a�@?}@@@�v@�e@~(@Xy@:�@@ݘ@�@�4@>�@�@��@�@��@xl@)�@�@�t@Q�@�@�U@��@��@e�@7@�@P�@�@�6@&�@�d@Y�@��@��@��@��@N�@2�@��@��@��@U�@6z@Y@��@�@҉@�\@c @C�@e@�@�'@2a@��@��@r�@9X@1'@/�@-�@	�@��@��@Z�@$t@��@҉@�h@��@�@5?@�#@�d@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�OA�ZQA�YA�U�A�U�A�Y�A�]�A�^5A�`�A�bA�bNA�dZA�e�A�d�A�g8A�gA�bNA�c A�c A�c�A�b�A�`�A�_;A�O�A�NA�L�A�B[A��A�&LAǼ�A�m)A�:�A�ɺA�?}Aö�A��A��A�q�A��>A�F?A�g�A�:�A�*�A�F�A�
�A�?�A�g�A�O�A�ҽA�w�A�A�h>A�1�A��vA���A���A�/�A���A�Z�A��A�� A��gA���A��NA�V�A���A��A�j�A�{A���A�+A���A�S�A�$tA��A��A�4�A�y�A�k�A��wA��'A���A�@�A�7�A�1�A���A��`A���A�!�A���A�m�A��&A�?A��OA�2aA�/�A�j�A}�XAy��Au��Asg8ArQ�Aq
�Am��Ak%FAf;dAc��Aah�A\N�AU��AM�*AJe,AG�zAA��A>˒A=�MA8ZA4h
A3�xA2�A2N<A1��A1ffA0�6A/�nA-t�A,��A,<6A,a|A,��A,�A,�A+K^A+:�A+\�A+e�A+_A+&�A*\�A)�NA)@OA(�A(qA'�IA&�!A&|�A&J�A%|�A% \A$�A#�^A#f�A"��A!��A ��A T�A*0A[WA�A��Av`A+A�fA�_A��A+A�A��AK^A�0Al�A7LA�A��A��A�oA�A8�A^�A�|A�FAv�A�HAXyA�jA��AK^A�A^�AMAqA_A��A1'AA�A �A@A��AN<A��A��ArGAO�A,�AYA
�*A
}VA
?A
4A
�A	�RA	I�A�A��AVA��A|�A�	AzxA-A
=A�XAVA�@A_�A�A�xA;dA��AO�A��AA�A%AC-A�aA�VAZA ��A �@�-w@�q@���@�c@��.@��@�q�@�W?@��'@��o@���@�ԕ@� \@���@���@���@��	@�@�@��6@�A�@��@�6z@���@��@�4@�C@�@��@��@�^5@��3@�hs@�J�@�*0@��@��@�Z@�J@�P@�=�@��,@�.@�L0@駇@��@�v�@���@�t@�"@�k�@�f�@�O@�A @��@��j@��@�Ɇ@䟾@�tT@�
�@㋬@�F@��@�n�@�*�@�M@�C�@ߏ�@�8@���@�v�@��@�.I@ܿ�@�H@�@�J#@څ�@�?@���@��
@ّh@�#�@ا�@��T@��@�g8@ճ�@�y�@Հ4@���@���@Ӏ4@�8@���@Ҥ�@�D�@�e@��
@�C�@��p@�	@�@�z�@�{@���@�r�@ˎ�@�;d@�C@��@ɍP@�	@ǳ�@�
=@Ɩ�@���@Ł�@�T�@���@�C�@�c@�O�@�4@�@�#:@���@�Q�@�:�@� i@��B@���@��@�dZ@���@�_�@�0U@��@���@���@�RT@���@�-@��@��o@��a@��@���@�(@�ȴ@���@��U@���@��\@�(�@��@��0@�:�@��@��@�q@�7�@�~@�	@��@��-@��4@�%F@���@��Y@�ff@�\�@�C-@�2�@��@��@�J�@�M@���@��@��@@��S@�!-@��$@�q@�_@��q@�s@�E9@���@��)@���@��O@��@�p;@�H@��@���@�a�@��@��@���@��@�'R@��j@��{@�S&@�+�@��M@�Ɇ@���@�|�@�:�@�˒@�@O@��@���@�r�@��@���@�f�@�C�@��@��}@�{@��n@���@� �@��&@���@�k�@��@���@�w�@�^5@�/�@��w@��@@�x@�;d@��@�҉@���@�s�@��@��h@�7L@���@���@�Q�@��@��#@��F@��=@�S&@�֡@�4@��@���@��7@�;d@��@��@��@��|@���@�bN@�;�@�,=@��@�w2@�T�@�4@���@��@�d�@�6�@���@��@�H�@��y@��+@�S�@�<�@���@��6@�{J@�6z@� i@���@��X@���@���@�_@��{@�o@���@�Q�@�ƨ@���@�j@��@��@��\@�p;@�p;@�e�@�=q@�	@�ԕ@��@��@�$t@�ѷ@��}@�l"@�;�@�!�@��@�w2@�q@���@���@�V�@�  @��f@�+@��f@��H@��$@���@�YK@���@�|�@�e�@�!-@���@�H�@�J@��3@�F�@��9@�a|@�%�@��^@�Q�@�33@�@��f@���@��Y@�6�@�.�@��9@���@�o�@�Dg@��@��9@�*�@��d@���@��"@�J�@��@�z@�($@��@��z@�g�@��H@��@�YK@�{@��@�@@�V@\)@~�@~l�@~ �@}J�@|�v@|�@|S�@|b@{�
@{�$@z�y@zGE@y��@y�3@y�7@y+�@y&�@x�@x�@w��@wK�@wY@v��@vc @v1�@vGE@vE�@v#:@u�7@uDg@u@@t��@t:�@t*�@t�@s�@st�@sC@r��@rp;@rO@qϫ@q��@qO�@p��@pz�@p6@p@o�	@oE9@n�@n1�@mϫ@m��@m=�@lU2@k�a@k!-@j�@j�!@jOv@i��@i \@h�@h�u@hC-@g��@gZ�@f�m@f4@e��@e5�@d��@d4n@c��@c� @c�6@cP�@b�@bL0@a�^@`�$@_��@_�*@_�f@__p@_4�@^��@^��@^d�@^)�@]�)@]ԕ@]��@]@@\��@\"h@[ƨ@[n/@[C@[�@Z��@Z{@Y�d@Yc�@Y�@X�9@W�w@V�b@V{@Ua�@T?�@S��@S�q@S|�@R�"@R�@QY�@Q�@P�@P�@P�@P>B@O��@OiD@N��@N��@N�m@NR�@M�o@M�^@M�~@Mw2@M`B@MB�@M�@L�@L�`@L�[@L�@LA�@K�r@K�g@J��@J^5@IrG@Iq@I�@H�	@H�E@H�4@Hj@H<�@G�A@G��@G�:@G��@Ge�@GO@F�X@FJ�@E�.@E�@E�C@E[W@E%F@D�@D�j@D��@D�o@D_@D'R@C�r@Cy�@B�B@Bq�@B-@A�'@Aa�@AJ�@A:�@@�@@1@?�F@?��@?��@>�@>E�@>6�@>!�@>{@=�)@=��@=\�@=-w@<��@<�@<y>@;��@;1�@:i�@9�9@9��@97L@8w�@7�$@7��@7x@6�@6p;@6@�@60U@6�@5�@5J�@4�@4�@3�f@3iD@3/�@2�M@2�h@2��@2��@21�@2�@1��@1��@1`B@1%@0��@0�@/�6@/��@/]�@/ i@.��@.q�@.O@.u@-�@,�@,e�@,1'@,G@+�q@+n/@+F�@+ i@*��@*��@*q�@*5?@*#:@)�.@)�Z@)�3@)�~@)�@(�$@(��@(!@'��@'>�@&�y@&�@&��@&�r@&s�@&�@%�@%��@%|@%5�@$�@$��@$g8@$>B@$%�@$�@#�a@#l�@#�@"ff@"GE@"5?@!�@!�n@!2a@!;@ �`@ ��@ ��@ ��@ ��@ |�@ oi@ [�@ 4n@   @�@�@��@U�@@�m@��@p;@YK@4@�N@�S@rG@a�@?}@@@�v@�e@~(@Xy@:�@@ݘ@�@�4@>�@�@��@�@��@xl@)�@�@�t@Q�@�@�U@��@��@e�@7@�@P�@�@�6@&�@�d@Y�@��@��@��@��@N�@2�@��@��@��@U�@6z@Y@��@�@҉@�\@c @C�@e@�@�'@2a@��@��@r�@9X@1'@/�@-�@	�@��@��@Z�@$t@��@҉@�h@��@�@5?@�#@�d@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
��B
�:B
�nB
��B
�:B
�TB
�nB
�nB
�nB
�nB
�nB
�nB
�nB
�nB
�nB
�nB
��B
��B
�&B
�&B
�@B
�@B
�@B
��B
��B
�B
��B
��B
��Ba�BjB��BՁB��B�<BBG�B}qB�B��B��B�DB�FB��B��B��B�B��B�B��B��B}qBrGBq�BgmBbhB_�B[	BY�BPbBM�BKBF�BH�BESB=�B.�B �B�B
�B��B�XB�&B�PB��B�BvFBX�BGEB:�B7�B(�B�B
�tB
�ZB
��B
��B
x�B
raB
mwB
fB
\�B
RoB
FtB
2�B
#B

�B	�B	��B	��B	�B	�nB	�eB	�B	t�B	WsB	G�B	7�B	]B��B�pB�+B�QB�B�B��B�eB�7B�;B�B��B	 �B	_B	=B	�B	B	�B	,�B	A B	UB	mB	u�B	�RB	�pB	��B	�$B	�wB	�	B	�dB	��B	�:B	��B	��B	�2B	��B	��B	�B
oB
 B	�B	�qB
 �B
�B
�B
-B
[B
 4B	��B	��B	��B
'B
�B
�B
{B
B
uB
uB
;B
�B
�B
B
�B
B
�B
�B
�B
AB
�B
�B
�B
�B
[B
�B
B
�B
GB
aB
aB
�B
B
�B
�B
B
B
[B
�B
gB
gB
aB
�B
'B
�B
�B
MB
�B
B
SB
SB
YB
�B
tB
mB
GB
�B
 4B	��B
 B	��B	��B
;B
 �B
;B
�B
oB
;B
�B
�B
;B
B	�HB	��B	��B	�0B	��B
zB

	B
0B
�B
B
 �B
AB
JB
B
~B

rB
	RB
�B
�B
	B
	�B
1B
�B
KB
EB
�B
�B
9B
�B
�B
�B
 �B	��B	��B	�B	�BB	�]B	��B
 4B
  B
 iB
  B
  B
  B
  B
 OB	��B	�(B	��B	��B	�B	�B	�6B	��B	�PB	�B	�B	�B	��B	��B	��B	�^B	�B	��B	��B	��B	��B	��B	��B	��B	�DB	��B	�rB	��B	�B	�B	��B	�RB	�rB	�XB	��B	�lB	��B	��B	�B	��B	��B	�FB	��B	�%B	�ZB	�+B	�`B	��B	�%B	�%B	�B	�-B	�B	�aB	��B	��B	�%B	��B	��B	��B	��B	��B	��B	��B	��B	�fB	��B	��B	�	B	��B	�xB	��B	��B	��B	�*B	�xB	��B	��B	�0B	�6B	��B	��B	�cB	�HB	�BB	��B	��B	�HB	��B
 �B
�B
�B
�B
�B
�B
-B
�B
�B
B
�B
;B
�B
�B
�B
�B
�B
�B
�B
�B
9B
?B
?B
�B
zB
�B
�B
�B
�B
�B
�B
�B
�B
�B
KB
�B
�B
�B
�B
�B
	B
	7B

	B

	B

XB

rB

rB

�B

XB
	�B
	�B

�B

rB
�B
�B
�B
�B
"B
�B
"B
"B
<B
�B
�B
B
B
�B
�B
B
B
(B
�B
�B
�B
�B
�B
�B
�B
bB
�B
�B
�B
�B
�B
�B
�B
�B
 B
�B
�B
B
:B
TB
�B
@B
uB
uB
�B
�B
B
B
2B
gB
�B
�B
�B
9B
SB
�B
SB
mB
$B

B
$B
YB
sB
�B
�B
�B
+B
yB
B
B
B
7B
QB
�B
�B
�B
�B
	B
B
)B
)B
�B
�B
�B
�B
xB
CB
�B
�B
�B
�B
�B
�B
�B
�B
B
OB
�B
�B
�B
!B
VB
�B
 'B
 \B
 BB
 �B
 �B
!�B
!�B
"4B
"NB
"NB
"hB
"NB
"NB
#�B
#�B
#�B
$�B
%FB
%`B
%zB
%�B
%�B
&�B
&�B
&fB
&fB
&�B
&�B
'B
&�B
'B
'8B
'8B
'mB
'�B
'�B
'�B
'�B
(sB
(�B
(�B
(�B
(�B
)*B
)�B
+B
+B
+B
+B
+6B
+kB
,�B
-B
,�B
-B
-wB
-�B
-�B
./B
.�B
.�B
.�B
.�B
/OB
0oB
1[B
1vB
1B
0�B
0�B
0�B
1�B
1vB
2�B
2|B
2�B
2�B
33B
3�B
3�B
3�B
3�B
4B
3�B
3�B
3�B
4B
4�B
5�B
49B
4nB
5ZB
5�B
6FB
6�B
6�B
7fB
7�B
7�B
8�B
9	B
9>B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
:B
:B
:DB
:�B
:^B
;0B
;0B
;JB
;dB
;B
;�B
;�B
<B
<�B
<�B
<�B
="B
=<B
=�B
>BB
>�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
?B
?.B
?B
?B
>�B
>wB
>wB
>�B
>�B
?cB
@4B
@B
@iB
@�B
@�B
@iB
@iB
@OB
@B
?}B
?HB
?�B
@�B
A;B
BB
BB
B�B
CGB
C{B
C{B
C�B
DMB
D�B
D�B
D�B
ESB
E�B
E�B
EmB
E9B
D�B
D�B
D�B
D�B
D�B
E9B
EmB
E�B
E�B
F%B
FB
FtB
FYB
F�B
GEB
G+B
G�B
G�B
G�B
G�B
G�B
HB
H1B
H�B
H�B
H�B
I�B
JrB
J�B
KxB
K�B
LB
L0B
L~B
L�B
MPB
M�B
MjB
M�B
M�B
M�B
NVB
NVB
NpB
NVB
NpB
N�B
O(B
OBB
OBB
OBB
OBB
O\B
OvB
OvB
O�B
O�B
O�B
PHB
P}B
PHB
PbB
PHB
Q�B
Q�B
R B
R B
R:B
RoB
RoB
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
T,B
T�B
T�B
T�B
U2B
U2B
UB
UB
T�B
T�B
T�B
UgB
U�B
U�B
V�B
V�B
V�B
V�B
V�B
W�B
W�B
W�B
W�B
W�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
YB
X�B
Y�B
Y�B
ZB
Z�B
Z�B
Z�B
[�B
\CB
\)B
\)B
\�B
\�B
]/B
]/B
]B
]/B
]�B
^B
^�B
_VB
_;B
_!B
_;B
_�B
_�B
_�B
_�B
`'B
`\B
`�B
`�B
`�B
aHB
a�B
a�B
a�B
b4B
bhB
bhB
bNB
bhB
b4B
bNB
a�B
a|B
abB
a-B
a|B
abB
abB
a�B
a�B
a�B
bB
bhB
b�B
b�B
bhB
b�B
b�B
c:B
cTB
cnB
c�B
d@B
d�B
eB
eB
e,B
e`B
ezB
e�B
e�B
fB
f2B
f�B
f�B
g8B
gRB
g�B
g�B
g�B
g�B
h>B
hsB
iDB
iDB
i_B
i�B
i�B
j0B
jeB
jB
jB
j�B
j�B
j�B
j�B
j�B
j�B
kB
k6B
k6B
kkB
kkB
k�B
l"B
lqB
l�B
l�B
l�B
l�B
m)B
m]B
mwB
m�B
m�B
m�B
m�B
nIB
n}B
n�B
n�B
n�B
oB
o5B
oiB
o�B
o�B
o�B
o�B
o�B
p!B
pUB
p�B
p�B
qAB
q[B
q�B
q�B
q�B
q�B
rGB
r�B
r�B
r�B
sMB
s�B
s�B
tTB
t�B
uB
uB
u%B
utB
u�B
u�B
u�B
v+B
v�B
v�B
v�B
v�B
wB
wB
wfB
w�B
w�B
w�B
x8B
xRB
x�B
y�B
yrB
y�B
y�B
y�B
y�B
y�B
zB
zDB
zxB
z�B
z�B
{0B
{dB
{B
{B
{B
|B
|PB
|6B
|P111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
��B
�:B
�nB
��B
�:B
�TB
�nB
�nB
�nB
�nB
�nB
�nB
�nB
�nB
�nB
�nB
��B
��B
�&B
�&B
�@B
�@B
�@B
��B
��B
�B
��B
��B
��Ba�BjB��BՁB��B�<BBG�B}qB�B��B��B�DB�FB��B��B��B�B��B�B��B��B}qBrGBq�BgmBbhB_�B[	BY�BPbBM�BKBF�BH�BESB=�B.�B �B�B
�B��B�XB�&B�PB��B�BvFBX�BGEB:�B7�B(�B�B
�tB
�ZB
��B
��B
x�B
raB
mwB
fB
\�B
RoB
FtB
2�B
#B

�B	�B	��B	��B	�B	�nB	�eB	�B	t�B	WsB	G�B	7�B	]B��B�pB�+B�QB�B�B��B�eB�7B�;B�B��B	 �B	_B	=B	�B	B	�B	,�B	A B	UB	mB	u�B	�RB	�pB	��B	�$B	�wB	�	B	�dB	��B	�:B	��B	��B	�2B	��B	��B	�B
oB
 B	�B	�qB
 �B
�B
�B
-B
[B
 4B	��B	��B	��B
'B
�B
�B
{B
B
uB
uB
;B
�B
�B
B
�B
B
�B
�B
�B
AB
�B
�B
�B
�B
[B
�B
B
�B
GB
aB
aB
�B
B
�B
�B
B
B
[B
�B
gB
gB
aB
�B
'B
�B
�B
MB
�B
B
SB
SB
YB
�B
tB
mB
GB
�B
 4B	��B
 B	��B	��B
;B
 �B
;B
�B
oB
;B
�B
�B
;B
B	�HB	��B	��B	�0B	��B
zB

	B
0B
�B
B
 �B
AB
JB
B
~B

rB
	RB
�B
�B
	B
	�B
1B
�B
KB
EB
�B
�B
9B
�B
�B
�B
 �B	��B	��B	�B	�BB	�]B	��B
 4B
  B
 iB
  B
  B
  B
  B
 OB	��B	�(B	��B	��B	�B	�B	�6B	��B	�PB	�B	�B	�B	��B	��B	��B	�^B	�B	��B	��B	��B	��B	��B	��B	��B	�DB	��B	�rB	��B	�B	�B	��B	�RB	�rB	�XB	��B	�lB	��B	��B	�B	��B	��B	�FB	��B	�%B	�ZB	�+B	�`B	��B	�%B	�%B	�B	�-B	�B	�aB	��B	��B	�%B	��B	��B	��B	��B	��B	��B	��B	��B	�fB	��B	��B	�	B	��B	�xB	��B	��B	��B	�*B	�xB	��B	��B	�0B	�6B	��B	��B	�cB	�HB	�BB	��B	��B	�HB	��B
 �B
�B
�B
�B
�B
�B
-B
�B
�B
B
�B
;B
�B
�B
�B
�B
�B
�B
�B
�B
9B
?B
?B
�B
zB
�B
�B
�B
�B
�B
�B
�B
�B
�B
KB
�B
�B
�B
�B
�B
	B
	7B

	B

	B

XB

rB

rB

�B

XB
	�B
	�B

�B

rB
�B
�B
�B
�B
"B
�B
"B
"B
<B
�B
�B
B
B
�B
�B
B
B
(B
�B
�B
�B
�B
�B
�B
�B
bB
�B
�B
�B
�B
�B
�B
�B
�B
 B
�B
�B
B
:B
TB
�B
@B
uB
uB
�B
�B
B
B
2B
gB
�B
�B
�B
9B
SB
�B
SB
mB
$B

B
$B
YB
sB
�B
�B
�B
+B
yB
B
B
B
7B
QB
�B
�B
�B
�B
	B
B
)B
)B
�B
�B
�B
�B
xB
CB
�B
�B
�B
�B
�B
�B
�B
�B
B
OB
�B
�B
�B
!B
VB
�B
 'B
 \B
 BB
 �B
 �B
!�B
!�B
"4B
"NB
"NB
"hB
"NB
"NB
#�B
#�B
#�B
$�B
%FB
%`B
%zB
%�B
%�B
&�B
&�B
&fB
&fB
&�B
&�B
'B
&�B
'B
'8B
'8B
'mB
'�B
'�B
'�B
'�B
(sB
(�B
(�B
(�B
(�B
)*B
)�B
+B
+B
+B
+B
+6B
+kB
,�B
-B
,�B
-B
-wB
-�B
-�B
./B
.�B
.�B
.�B
.�B
/OB
0oB
1[B
1vB
1B
0�B
0�B
0�B
1�B
1vB
2�B
2|B
2�B
2�B
33B
3�B
3�B
3�B
3�B
4B
3�B
3�B
3�B
4B
4�B
5�B
49B
4nB
5ZB
5�B
6FB
6�B
6�B
7fB
7�B
7�B
8�B
9	B
9>B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
:B
:B
:DB
:�B
:^B
;0B
;0B
;JB
;dB
;B
;�B
;�B
<B
<�B
<�B
<�B
="B
=<B
=�B
>BB
>�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
?B
?.B
?B
?B
>�B
>wB
>wB
>�B
>�B
?cB
@4B
@B
@iB
@�B
@�B
@iB
@iB
@OB
@B
?}B
?HB
?�B
@�B
A;B
BB
BB
B�B
CGB
C{B
C{B
C�B
DMB
D�B
D�B
D�B
ESB
E�B
E�B
EmB
E9B
D�B
D�B
D�B
D�B
D�B
E9B
EmB
E�B
E�B
F%B
FB
FtB
FYB
F�B
GEB
G+B
G�B
G�B
G�B
G�B
G�B
HB
H1B
H�B
H�B
H�B
I�B
JrB
J�B
KxB
K�B
LB
L0B
L~B
L�B
MPB
M�B
MjB
M�B
M�B
M�B
NVB
NVB
NpB
NVB
NpB
N�B
O(B
OBB
OBB
OBB
OBB
O\B
OvB
OvB
O�B
O�B
O�B
PHB
P}B
PHB
PbB
PHB
Q�B
Q�B
R B
R B
R:B
RoB
RoB
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
T,B
T�B
T�B
T�B
U2B
U2B
UB
UB
T�B
T�B
T�B
UgB
U�B
U�B
V�B
V�B
V�B
V�B
V�B
W�B
W�B
W�B
W�B
W�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
YB
X�B
Y�B
Y�B
ZB
Z�B
Z�B
Z�B
[�B
\CB
\)B
\)B
\�B
\�B
]/B
]/B
]B
]/B
]�B
^B
^�B
_VB
_;B
_!B
_;B
_�B
_�B
_�B
_�B
`'B
`\B
`�B
`�B
`�B
aHB
a�B
a�B
a�B
b4B
bhB
bhB
bNB
bhB
b4B
bNB
a�B
a|B
abB
a-B
a|B
abB
abB
a�B
a�B
a�B
bB
bhB
b�B
b�B
bhB
b�B
b�B
c:B
cTB
cnB
c�B
d@B
d�B
eB
eB
e,B
e`B
ezB
e�B
e�B
fB
f2B
f�B
f�B
g8B
gRB
g�B
g�B
g�B
g�B
h>B
hsB
iDB
iDB
i_B
i�B
i�B
j0B
jeB
jB
jB
j�B
j�B
j�B
j�B
j�B
j�B
kB
k6B
k6B
kkB
kkB
k�B
l"B
lqB
l�B
l�B
l�B
l�B
m)B
m]B
mwB
m�B
m�B
m�B
m�B
nIB
n}B
n�B
n�B
n�B
oB
o5B
oiB
o�B
o�B
o�B
o�B
o�B
p!B
pUB
p�B
p�B
qAB
q[B
q�B
q�B
q�B
q�B
rGB
r�B
r�B
r�B
sMB
s�B
s�B
tTB
t�B
uB
uB
u%B
utB
u�B
u�B
u�B
v+B
v�B
v�B
v�B
v�B
wB
wB
wfB
w�B
w�B
w�B
x8B
xRB
x�B
y�B
yrB
y�B
y�B
y�B
y�B
y�B
zB
zDB
zxB
z�B
z�B
{0B
{dB
{B
{B
{B
|B
|PB
|6B
|P111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220720154043  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220720154218  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220720154219  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220720154219                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220721004224  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220721004224  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220720155828                      G�O�G�O�G�O�                