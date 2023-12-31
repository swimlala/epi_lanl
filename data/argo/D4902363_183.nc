CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-11-27T00:35:28Z creation;2017-11-27T00:35:31Z conversion to V3.1;2019-12-19T07:55:41Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        p  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \X   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  `4   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  �d   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  �D   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ۴   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �D   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �D   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �D   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �D   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �$   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �(   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �8   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �<   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �@   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  20171127003528  20200115121517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_183                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�8u��� 1   @�8vhK� @;���w�k�d` ѷY1   GPS     A   B   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A~ffA�  A���A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B?��BH  BP  BX  B`  Bh  Bp  Bw��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2y�D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_�fD`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�L�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@���@�p�@�p�A�RA>�RA^�RA}�A�\)A�(�A�\)A�\)A�\)A�\)A�\)A�\)B�B�B�B�B'�B/�B7�B?G�BG�BO�BW�B_�Bg�Bo�BwG�B�B��
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
B���B��
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
B��
C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D	z�D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D#��D$z�D$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2t{D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_�HD_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtz�Dt��Duz�Du��Dvz�Dv��Dwz�Dw��Dxz�Dx��Dyz�Dy��Dzz�Dz��D{z�D{��D|z�D|��D}z�D}��D~z�D~��Dz�D��D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD½qD��qD�=qD�}qDýqD��qD�=qD�}qDĽqD��qD�=qD�}qDŽqD��qD�=qD�}qDƽqD��qD�=qD�}qDǽqD��qD�=qD�}qDȽqD��qD�=qD�}qDɽqD��qD�=qD�}qDʽqD��qD�=qD�}qD˽qD��qD�=qD�}qD̽qD��qD�=qD�}qDͽqD��qD�=qD�}qDνqD��qD�=qD�}qDϽqD��qD�=qD�}qDнqD��qD�=qD�}qDѽqD��qD�=qD�}qDҽqD��qD�=qD�}qDӽqD��qD�=qD�}qDԽqD��qD�=qD�}qDսqD��qD�=qD�}qDֽqD��qD�=qD�}qD׽qD��qD�=qD�}qDؽqD��qD�=qD�}qDٽqD��qD�=qD�}qDڽqD��qD�=qD�}qD۽qD��qD�=qD�}qDܽqD��qD�=qD�}qDݽqD��qD�=qD�}qD޽qD��qD�=qD�}qD߽qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�J=1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�S�A�Q�A�S�A�VA�VA�XA�XA�ZA�ZA�ZA�VA�VA�XA�ZA�\)A�\)A�ZA�ZA�\)A�`BA�`BA�bNA�dZA�dZA�dZA�dZA�bNA�S�A�S�A�S�A�ZA�ZA�bNA�^5A�\)A�XA�XA�ZA�VA�S�A�K�A�9XA�9XA�-A��A�bNA��/A���A��A�t�A���A��yA��!A��PA�ȴA���A���A�
=A�VA���A��yA�|�A��A��9A�$�A��HA�r�A���A�hsA���A�C�A�%A��A��FA��A�jA��A��uA��/A���A�=qA��A��
A���A��RA��A��uA��A�(�A~  A|A�A{/Az�Ax��Awx�Av�9AvA�Au�At��Arr�Aq��ApjAo�PAn1'Al�+AkO�Ai��Ah��Ag��Af�+Ae�FAe�Ad��Adn�Act�AbĜAb��Ab$�Aa�A`�+A]�A\�A[VAZQ�AY�^AY
=AX�AV~�AV  AU&�AT�\AT�AS��ASoARr�AQt�APȴAO\)ANM�AL�jAKG�AJ�DAJE�AI&�AG��AF�AF1'AE��AE�FAEl�AE?}AD�AD~�AC��AB��ABI�AA�A@~�A?�wA=�A;��A9ƨA9
=A8E�A6��A6-A5ƨA4��A4~�A3�TA3|�A2ȴA1�TA1%A0�RA0~�A/l�A.��A.(�A-33A,Q�A,JA+G�A*�/A*��A*ZA);dA(Q�A'p�A%%A$A�A#��A#�A#t�A#O�A#�A"ȴA"n�A!��A!K�A �jA�AC�A�!A�+AjA9XA�A��At�AĜA�yA�A��A��AO�A�A��An�AA
=A9XAt�Ap�A{A+AE�A��A(�A
��A�A�A/A�/A��AVA  AA�jA�+AAS�A�A �A ZA b@���@�dZ@�C�@���@�G�@�A�@�  @���@���@�~�@�n�@�v�@��\@�ff@��@�7L@�I�@��@�;d@�  @�G�@��@�%@�G�@�A�@�@߮@ް!@���@��@�K�@��m@�E�@ԓu@�
=@���@�O�@�V@�%@���@У�@�A�@��
@�\)@Χ�@�X@�Q�@���@�l�@ʟ�@�?}@�I�@Ǯ@���@��T@ě�@��y@�J@�b@��\@�X@� �@��w@�C�@���@���@�x�@��@�o@�{@��@�A�@�
=@�@���@�%@��@��w@��@���@��T@�G�@���@�I�@��m@�|�@�t�@�;d@��@�~�@�V@��-@�G�@�%@���@��@�K�@���@��9@�Q�@��@�dZ@�K�@�
=@�o@��m@��w@�C�@�-@�r�@�;d@�J@��@�|�@�"�@��!@��R@��+@��@��/@���@��`@�  @��P@�S�@���@��@���@��7@��@��@�p�@��`@���@���@��@�bN@�Z@�1@��;@��P@�\)@��@�-@��^@�O�@�hs@���@��/@�9X@���@�l�@�S�@�S�@�@��y@�ȴ@�J@�{@�J@�J@�{@�p�@�7L@��/@�bN@�A�@�l�@�ff@�@���@��@�K�@���@�v�@�J@��@��T@���@�x�@��@��/@��D@�bN@�  @��@���@��@�t�@�C�@�1'@�S�@���@�^5@���@�E�@���@��D@�Z@�(�@��@~ȴ@~�+@~�+@~�+@~v�@~V@~E�@~5?@~$�@~$�@}�@}�@}�T@}`B@|��@|�D@|I�@{�F@{33@z��@z=q@y7L@xA�@x �@x  @wK�@w;d@wK�@wK�@w\)@w\)@wl�@wl�@w|�@w\)@v��@u��@t�@s"�@rJ@q��@qhs@q&�@p��@p�`@pr�@o�w@nȴ@n��@n$�@n@m�-@m�h@mO�@l��@k�m@kt�@k33@k��@lI�@lI�@lZ@lj@lz�@lj@l(�@k��@k�m@k�m@k�
@k�
@kƨ@k��@kdZ@ko@j��@j^5@i��@i��@ix�@i�@h�`@h��@i%@h��@h��@h��@i�@iX@h��@hb@g�@fv�@f5?@e?}@dz�@c"�@b��@bM�@b�@dj@d�D@d��@eV@e/@e/@e/@d�@dj@dj@d��@dj@cƨ@b�H@b=q@a��@a��@a�7@a�7@a�7@ax�@ax�@ahs@ahs@ahs@a7L@`�`@`��@`�u@`A�@` �@_�w@_�@^ȴ@^ff@^E�@^$�@^{@]�T@]p�@\Z@\1@[�
@[��@[C�@Z�\@Y�^@Y�^@Y��@Y��@Y��@Y�^@Y��@X�9@W��@W�P@W\)@Vff@U��@UV@T�D@T�D@S��@R��@R�@R-@Q�7@P�@O��@O\)@N�R@M@M��@M��@M��@M��@M��@MO�@LZ@Kƨ@K��@KC�@J�\@J-@I��@I�7@I�^@I��@I�#@I&�@H��@H��@H�u@G�@G|�@GK�@Fv�@E�T@E�-@E��@E@E�@Ep�@E`B@EO�@E?}@EV@C�
@C"�@B�\@A��@A&�@A%@@Ĝ@@�9@@�9@@�u@@�@@�@@r�@@r�@@Q�@?�@?�@?K�@?;d@?
=@>�y@>ȴ@>��@>��@>�+@>V@>E�@>5?@>$�@=@=��@=�@=p�@<�@<�j@<��@<9X@<�@;�
@;��@;t�@;o@:�H@:��@:�!@:^5@9�@9�7@9hs@9hs@9X@9G�@9G�@9&�@9%@8��@8A�@8  @7�;@7��@7�P@7l�@7;d@6��@6�@6�R@6��@6��@6�+@65?@6$�@5�@5@5p�@5?}@4�D@4(�@41@3�
@3S�@3"�@3@2��@2��@2�\@2n�@2^5@2M�@2=q@2�@1��@1�#@1�^@1�^@1��@1x�@1G�@1�@0�`@0r�@0A�@0b@/�w@/�P@/\)@/+@/
=@.ȴ@.��@.�+@-�T@-�@-?}@,�@,j@+�
@+��@+"�@)��@)G�@)&�@)&�@)&�@(��@(Ĝ@(��@(Ĝ@(Q�@'�@'�P@'|�@'l�@'\)@'+@&��@%��@%��@%V@$��@$��@$��@$��@#�F@#��@#��@#��@#��@#C�@"�H@"�!@"n�@"=q@"J@!�#@!��@!��@!hs@!7L@!&�@!%@ r�@ 1'@  �@ b@��@�P@;d@
=@�y@ȴ@�R@5?@{@@�T@�@/@�@�@�D@I�@(�@�@��@�m@�
@t�@�!@J@�#@��@��@�^@��@&�@�u@Q�@  @�w@K�@;d@+@+@+@+@+@ȴ@v�@E�@@��@��@�-@�h@�h@�@p�@O�@V@��@�D@z�@j@9X@(�@1@�
@��@��@��@�@"�@��@�\@n�@^5@M�@M�@=q@-@J@��@�@�#@��@hs@%@Ĝ@r�@1'@ �@�@��@�w@�w@�@�P@l�@�@ȴ@��@��@v�@v�@ff@@�T@�-@�h@p�@V@�@�@Z@�@�
@�
@�
@�
@ƨ@��@dZ@o@
^5@
�@	�@	��@	hs@	G�@	7L@	7L@	&�@	%@��@��@Ĝ@�@b@�w@�@��@�P@l�@\)@K�@�@��@�y@�@�R@��@�+@E�@{@{@@��@�-@�-@`B@�@�/@��@(�@�m@�F@��@t�@C�@@��@��@��@��1111111111111111111111111111111111111111111144411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�S�A�Q�A�S�A�VA�VA�XA�XA�ZA�ZA�ZA�VA�VA�XA�ZA�\)A�\)A�ZA�ZA�\)A�`BA�`BA�bNA�dZA�dZA�dZA�dZA�bNA�S�A�S�A�S�A�ZA�ZA�bNA�^5A�\)A�XA�XA�ZA�VA�S�A�K�A�9XA�9XA�-G�O�G�O�G�O�A���A��A�t�A���A��yA��!A��PA�ȴA���A���A�
=A�VA���A��yA�|�A��A��9A�$�A��HA�r�A���A�hsA���A�C�A�%A��A��FA��A�jA��A��uA��/A���A�=qA��A��
A���A��RA��A��uA��A�(�A~  A|A�A{/Az�Ax��Awx�Av�9AvA�Au�At��Arr�Aq��ApjAo�PAn1'Al�+AkO�Ai��Ah��Ag��Af�+Ae�FAe�Ad��Adn�Act�AbĜAb��Ab$�Aa�A`�+A]�A\�A[VAZQ�AY�^AY
=AX�AV~�AV  AU&�AT�\AT�AS��ASoARr�AQt�APȴAO\)ANM�AL�jAKG�AJ�DAJE�AI&�AG��AF�AF1'AE��AE�FAEl�AE?}AD�AD~�AC��AB��ABI�AA�A@~�A?�wA=�A;��A9ƨA9
=A8E�A6��A6-A5ƨA4��A4~�A3�TA3|�A2ȴA1�TA1%A0�RA0~�A/l�A.��A.(�A-33A,Q�A,JA+G�A*�/A*��A*ZA);dA(Q�A'p�A%%A$A�A#��A#�A#t�A#O�A#�A"ȴA"n�A!��A!K�A �jA�AC�A�!A�+AjA9XA�A��At�AĜA�yA�A��A��AO�A�A��An�AA
=A9XAt�Ap�A{A+AE�A��A(�A
��A�A�A/A�/A��AVA  AA�jA�+AAS�A�A �A ZA b@���@�dZ@�C�@���@�G�@�A�@�  @���@���@�~�@�n�@�v�@��\@�ff@��@�7L@�I�@��@�;d@�  @�G�@��@�%@�G�@�A�@�@߮@ް!@���@��@�K�@��m@�E�@ԓu@�
=@���@�O�@�V@�%@���@У�@�A�@��
@�\)@Χ�@�X@�Q�@���@�l�@ʟ�@�?}@�I�@Ǯ@���@��T@ě�@��y@�J@�b@��\@�X@� �@��w@�C�@���@���@�x�@��@�o@�{@��@�A�@�
=@�@���@�%@��@��w@��@���@��T@�G�@���@�I�@��m@�|�@�t�@�;d@��@�~�@�V@��-@�G�@�%@���@��@�K�@���@��9@�Q�@��@�dZ@�K�@�
=@�o@��m@��w@�C�@�-@�r�@�;d@�J@��@�|�@�"�@��!@��R@��+@��@��/@���@��`@�  @��P@�S�@���@��@���@��7@��@��@�p�@��`@���@���@��@�bN@�Z@�1@��;@��P@�\)@��@�-@��^@�O�@�hs@���@��/@�9X@���@�l�@�S�@�S�@�@��y@�ȴ@�J@�{@�J@�J@�{@�p�@�7L@��/@�bN@�A�@�l�@�ff@�@���@��@�K�@���@�v�@�J@��@��T@���@�x�@��@��/@��D@�bN@�  @��@���@��@�t�@�C�@�1'@�S�@���@�^5@���@�E�@���@��D@�Z@�(�@��@~ȴ@~�+@~�+@~�+@~v�@~V@~E�@~5?@~$�@~$�@}�@}�@}�T@}`B@|��@|�D@|I�@{�F@{33@z��@z=q@y7L@xA�@x �@x  @wK�@w;d@wK�@wK�@w\)@w\)@wl�@wl�@w|�@w\)@v��@u��@t�@s"�@rJ@q��@qhs@q&�@p��@p�`@pr�@o�w@nȴ@n��@n$�@n@m�-@m�h@mO�@l��@k�m@kt�@k33@k��@lI�@lI�@lZ@lj@lz�@lj@l(�@k��@k�m@k�m@k�
@k�
@kƨ@k��@kdZ@ko@j��@j^5@i��@i��@ix�@i�@h�`@h��@i%@h��@h��@h��@i�@iX@h��@hb@g�@fv�@f5?@e?}@dz�@c"�@b��@bM�@b�@dj@d�D@d��@eV@e/@e/@e/@d�@dj@dj@d��@dj@cƨ@b�H@b=q@a��@a��@a�7@a�7@a�7@ax�@ax�@ahs@ahs@ahs@a7L@`�`@`��@`�u@`A�@` �@_�w@_�@^ȴ@^ff@^E�@^$�@^{@]�T@]p�@\Z@\1@[�
@[��@[C�@Z�\@Y�^@Y�^@Y��@Y��@Y��@Y�^@Y��@X�9@W��@W�P@W\)@Vff@U��@UV@T�D@T�D@S��@R��@R�@R-@Q�7@P�@O��@O\)@N�R@M@M��@M��@M��@M��@M��@MO�@LZ@Kƨ@K��@KC�@J�\@J-@I��@I�7@I�^@I��@I�#@I&�@H��@H��@H�u@G�@G|�@GK�@Fv�@E�T@E�-@E��@E@E�@Ep�@E`B@EO�@E?}@EV@C�
@C"�@B�\@A��@A&�@A%@@Ĝ@@�9@@�9@@�u@@�@@�@@r�@@r�@@Q�@?�@?�@?K�@?;d@?
=@>�y@>ȴ@>��@>��@>�+@>V@>E�@>5?@>$�@=@=��@=�@=p�@<�@<�j@<��@<9X@<�@;�
@;��@;t�@;o@:�H@:��@:�!@:^5@9�@9�7@9hs@9hs@9X@9G�@9G�@9&�@9%@8��@8A�@8  @7�;@7��@7�P@7l�@7;d@6��@6�@6�R@6��@6��@6�+@65?@6$�@5�@5@5p�@5?}@4�D@4(�@41@3�
@3S�@3"�@3@2��@2��@2�\@2n�@2^5@2M�@2=q@2�@1��@1�#@1�^@1�^@1��@1x�@1G�@1�@0�`@0r�@0A�@0b@/�w@/�P@/\)@/+@/
=@.ȴ@.��@.�+@-�T@-�@-?}@,�@,j@+�
@+��@+"�@)��@)G�@)&�@)&�@)&�@(��@(Ĝ@(��@(Ĝ@(Q�@'�@'�P@'|�@'l�@'\)@'+@&��@%��@%��@%V@$��@$��@$��@$��@#�F@#��@#��@#��@#��@#C�@"�H@"�!@"n�@"=q@"J@!�#@!��@!��@!hs@!7L@!&�@!%@ r�@ 1'@  �@ b@��@�P@;d@
=@�y@ȴ@�R@5?@{@@�T@�@/@�@�@�D@I�@(�@�@��@�m@�
@t�@�!@J@�#@��@��@�^@��@&�@�u@Q�@  @�w@K�@;d@+@+@+@+@+@ȴ@v�@E�@@��@��@�-@�h@�h@�@p�@O�@V@��@�D@z�@j@9X@(�@1@�
@��@��@��@�@"�@��@�\@n�@^5@M�@M�@=q@-@J@��@�@�#@��@hs@%@Ĝ@r�@1'@ �@�@��@�w@�w@�@�P@l�@�@ȴ@��@��@v�@v�@ff@@�T@�-@�h@p�@V@�@�@Z@�@�
@�
@�
@�
@ƨ@��@dZ@o@
^5@
�@	�@	��@	hs@	G�@	7L@	7L@	&�@	%@��@��@Ĝ@�@b@�w@�@��@�P@l�@\)@K�@�@��@�y@�@�R@��@�+@E�@{@{@@��@�-@�-@`B@�@�/@��@(�@�m@�F@��@t�@C�@@��@��@��@��1111111111111111111111111111111111111111111144411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BuBhBDB��B�#B�1Bs�B��B��B�3B�B�!B��B�7B�Bp�B\)BA�BA�B?}B0!B$�B!�B�B�B{B
=B
��B
�B
��B
�B
�sB
�B
��B
�^B
�RB
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
�PB
v�B
o�B
jB
hsB
YB
ZB
VB
R�B
N�B
C�B
/B
1'B
'�B
 �B
{B
	7B
B	��B	��B	�B	�B	�B	�yB	�yB	�ZB	�5B	�B	�)B	�B	��B	ƨB	�'B	�B	��B	��B	��B	��B	��B	�PB	�VB	�1B	�%B	�B	�B	{�B	v�B	o�B	iyB	`BB	W
B	P�B	L�B	L�B	M�B	D�B	;dB	7LB	>wB	<jB	<jB	:^B	9XB	7LB	33B	-B	(�B	%�B	!�B	�B	{B	+B��B�B�B�B�yB�fB�mB�TB�ZB�;B�/B�B��B��B��B��BɺBǮBȴBB�}BB�wB�jB�jB�LB�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�oB�oB��B��B�{B�oB�hB�PB�By�B{�By�Bv�B�B� B}�B{�Bw�Bu�Bq�Bo�BcTB_;Be`BaHB^5BT�BQ�BF�BG�BJ�BJ�BJ�BH�BE�BA�BF�BG�BA�B=qB6FB5?B=qBB�BD�BE�BD�B@�B>wBB�BE�BB�B;dB33BG�BJ�BK�BK�BJ�BH�BG�BG�B@�B2-B+B$�B�B�B#�B!�B$�B+B+B'�B!�B�B%�B(�B+B1'B5?B9XB:^B:^B7LB7LB7LB6FB33B1'B49B8RB7LB6FB33B9XB:^B:^B8RB7LB7LB<jB8RB8RB=qB@�BF�BF�BD�BD�BD�BB�B>wBA�BA�BC�BA�BD�BH�BH�BH�BM�BR�BQ�BVBXBZB\)B^5B_;BcTBaHBaHBcTBffBffBk�Bo�Bn�Bl�BiyBe`Bm�Bs�Bw�Bw�Bz�B{�B� B�+B�bB�uB�bB�+B�1B�B~�B� B�B�%B�=B�hB�oB�oB��B��B��B��B��B��B��B�B�3B�XB�XB�XB�RB��BƨBǮBǮBȴBɺB��B��B��B��B��B��B�B�)B�;B�5B�BB�ZB�mB�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B	B	B	1B	DB	DB	
=B		7B		7B	JB	\B	hB	hB	{B	�B	�B	�B	�B	�B	�B	�B	!�B	%�B	%�B	#�B	(�B	,B	.B	.B	1'B	7LB	9XB	9XB	9XB	9XB	:^B	:^B	:^B	;dB	:^B	;dB	;dB	:^B	<jB	?}B	@�B	A�B	E�B	J�B	K�B	N�B	Q�B	W
B	W
B	W
B	\)B	_;B	_;B	_;B	_;B	_;B	_;B	_;B	`BB	aHB	dZB	dZB	gmB	gmB	jB	k�B	l�B	l�B	m�B	m�B	n�B	p�B	u�B	w�B	y�B	y�B	{�B	{�B	|�B	� B	�B	�+B	�=B	�bB	�uB	�{B	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�-B	�3B	�3B	�FB	�LB	�?B	�?B	�?B	�LB	�LB	�XB	�XB	�}B	��B	ĜB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�/B	�;B	�BB	�NB	�NB	�TB	�TB	�NB	�TB	�TB	�TB	�NB	�NB	�NB	�NB	�NB	�NB	�NB	�NB	�HB	�TB	�TB	�ZB	�`B	�`B	�`B	�ZB	�TB	�mB	�mB	�mB	�fB	�`B	�fB	�B	�B	�B	�B	�B	�yB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
%B
+B
+B
%B
%B
+B
+B
+B
	7B

=B

=B

=B
DB
JB
DB
DB
DB
	7B
DB
JB
JB
VB
bB
bB
hB
oB
oB
oB
uB
uB
uB
oB
oB
hB
hB
oB
oB
uB
uB
uB
{B
�B
{B
�B
�B
{B
{B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
"�B
"�B
"�B
#�B
$�B
$�B
$�B
$�B
#�B
$�B
$�B
$�B
#�B
$�B
#�B
$�B
%�B
%�B
$�B
%�B
&�B
&�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
,B
,B
-B
-B
-B
.B
.B
0!B
0!B
/B
/B
0!B
0!B
0!B
0!B
2-B
1'B
/B
2-B
49B
49B
5?B
5?B
6FB
7LB
7LB
7LB
6FB
9XB
9XB
9XB
9XB
8RB
7LB
7LB
8RB
7LB
8RB
:^B
9XB
9XB
8RB
;dB
<jB
<jB
<jB
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
?}B
?}B
?}B
@�B
A�B
A�B
@�B
A�B
A�B
B�B
C�B
C�B
C�B
C�B
D�B
E�B
E�B
D�B
E�B
F�B
F�B
E�B
G�B
G�B
H�B
H�B
H�B
G�B
F�B
F�B
H�B
J�B
K�B
K�B
K�B
J�B
J�B
J�B
L�B
L�B
M�B
M�B
O�B
O�B
O�B
O�B
O�B
O�B
N�B
O�B
P�B
O�B
P�B
Q�B
R�B
R�B
S�B
S�B
S�B
S�B
R�B
R�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
VB
VB
VB
T�B
VB
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
XB
XB
XB
XB
XB
YB
ZB
ZB
[#B
[#B
[#B
\)B
\)B
\)B
[#B
[#B
[#B
\)B
]/B
]/B
]/B
]/B
]/B
\)B
]/B
^5B
^5B
^5B
]/B
^5B
_;B
^5B
_;B
`BB
aHB
aHB
`BB
`BB
`BB
`BB
_;B
_;B
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
cTB
cTB
cTB
cTB
e`B
e`B
e`B
dZB
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
gmB
gmB
ffB
gmB
gmB
ffB
ffB
gmB
gmB
gmB
hsB
hsB
iyB
iyB
iyB
iyB
jB
k�B
k�B
k�B
k�1111111111111111111111111111111111111111111144411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BoB�B�G�O�G�O�G�O�B�B�B��B�B��B�`B�B��Bs�B`BFBD�BA�B33B'B#�B�BxB�B0B
��B
�nB
��B
�vB
�B
�)B
��B
�B
��B
��B
�tB
��B
��B
�|B
�B
��B
��B
�B
�B
�B
��B
y�B
q�B
k�B
i�B
Z�B
[WB
V�B
S�B
O�B
E9B
1�B
2aB
)_B
"4B
mB
DB
�B	��B	�fB	�%B	��B	�qB	�KB	��B	�,B	�VB	��B	ܒB	��B	��B	�fB	�TB	�5B	�KB	��B	��B	��B	��B	�(B	�B	�7B	��B	��B	��B	|�B	w�B	p�B	j�B	a�B	X�B	R�B	NpB	M�B	NpB	F%B	=<B	8�B	>�B	<�B	<�B	:�B	9�B	7�B	4B	./B	)�B	&�B	#B	#B	B		�B��B�B��B��B�B�mB�$B�tB�B�B��B�B�2B�B�uBңB�)B��BɆB��B��B�B�cB�B�B�B��B��B�sB�1B��B��B�B�B�,B�:B�\B�OB��B�eB�mB��B�[B�&B��B��B��B��B��B�<B�tB|6B}qB{BxlB�oB�iB~�B|�Bx�Bv�Br�BqBe�Ba-Bf�Bb�B_pBW$BTBIlBH�BKDBKDBKDBIlBFtBB�BG+BH1BB�B>�B8B6�B>(BCBEBE�BEBAUB?}BCaBFBC-B<�B5%BG�BJ�BK�BL0BK^BIRBHfBHBA�B4nB,�B&�B \B�B$�B#�B&2B+�B+�B(�B#:B�B'B*0B,"B1�B5�B9�B:xB:�B7�B7�B7�B6�B3�B2-B4�B8�B7�B7B4TB:*B;B;0B9>B8RB8�B=VB9�B9�B>wBAUBF�BGBE9BESBEBCaB?�BB[BBuBDMBBuBESBI7BIRBI�BN<BSuBR�BV�BX�BZ�B\�B^�B_�BcnBa�Ba�Bc�Bf�Bf�Bk�BpBoBm)BjeBf�Bn/BtBxBxRB{B|6B� B��B��B��B�hB��B�7B�'B� B��B�mB�tB��B��B��B�&B��B��B�KB�)B�-B�vB�yB�iB��B�XB�rB��B��B��B��B��B��B��B�	B�B�"B�NB�[B�oB�aB�yB�CB�pB��B��B��B�B�B�B��B��B��B�!B��B��B��B��B�FB�*B�0B�PB�]B��B��B��B��B��B��B	�B	mB	�B	^B	^B	
�B		�B		�B	~B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 \B	 BB	"B	%�B	&fB	$�B	)_B	,=B	.cB	.cB	1�B	7�B	9XB	9�B	9rB	9rB	:�B	:xB	:xB	;dB	:�B	;B	;�B	:�B	<�B	?�B	@�B	A�B	E�B	J�B	LB	OBB	R:B	W$B	W$B	W?B	\)B	_;B	_VB	_;B	_VB	_;B	_VB	_VB	`vB	a�B	d�B	d�B	g�B	g�B	j�B	k�B	l�B	l�B	m�B	m�B	o B	qB	u�B	xB	y�B	zB	|B	|6B	}<B	�OB	�MB	�EB	�=B	�HB	�uB	�{B	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�6B	�QB	�)B	�/B	�B	�;B	�GB	�3B	�MB	�`B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�gB	�XB	��B	ˬB	��B	��B	��B	�B	�"B	�B	�$B	�+B	�KB	چB	�~B	�pB	�vB	�B	�B	�TB	�TB	�hB	�nB	�TB	�nB	�B	�hB	�hB	�hB	�B	�B	�hB	�B	�B	�B	�nB	�tB	�B	�zB	�zB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�-B	��B	�B	��B	�B	�B	�B	��B	�B	�B	��B	��B	��B	��B	�	B	�B	�2B	�*B	��B	�B	�6B	�(B	�B
 B
B
3B
3B
{B
?B
_B
EB
tB
YB
_B
�B
_B
	RB

XB

rB

XB
^B
dB
^B
xB
xB
	�B
�B
�B
�B
�B
}B
}B
�B
�B
�B
�B
�B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
#B
"�B
"�B
#�B
$�B
$�B
$�B
%B
#�B
$�B
%B
%B
$&B
%B
$B
%B
&B
%�B
%B
&B
'B
'B
($B
'�B
(
B
(�B
)B
)B
)*B
)B
)B
*B
)�B
*B
*B
*B
*B
*B
*0B
+6B
,=B
,=B
-CB
-CB
-CB
./B
.IB
0UB
0UB
/OB
/iB
0UB
0UB
0oB
0UB
2GB
1vB
/�B
2aB
4TB
4TB
5?B
5tB
6`B
7LB
7fB
7�B
6�B
9rB
9XB
9rB
9rB
8lB
7�B
7�B
8lB
7�B
8lB
:xB
9rB
9rB
8�B
;B
<�B
<�B
<�B
<�B
<�B
>�B
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
@�B
A�B
A�B
@�B
A�B
A�B
B�B
C�B
C�B
C�B
C�B
D�B
E�B
E�B
D�B
E�B
F�B
F�B
E�B
G�B
G�B
H�B
H�B
H�B
G�B
F�B
GB
IB
J�B
K�B
K�B
K�B
J�B
J�B
J�B
L�B
MB
M�B
N"B
O�B
O�B
O�B
O�B
O�B
PB
O(B
PB
QB
O�B
QB
Q�B
R�B
SB
S�B
S�B
T,B
TB
S&B
SB
UB
T�B
UB
U2B
UB
UB
UB
VB
VB
VB
V9B
U2B
VB
W?B
W$B
XB
X+B
XB
XB
X+B
X+B
X+B
XB
XEB
X+B
X+B
X+B
Y1B
ZQB
Z7B
[=B
[=B
[=B
\)B
\)B
\CB
[WB
[WB
[=B
\CB
]dB
]/B
]dB
]IB
]dB
\]B
]IB
^jB
^jB
^OB
]IB
^OB
_pB
^�B
_pB
`\B
abB
abB
`\B
`vB
`\B
`\B
_pB
_�B
abB
b�B
b�B
b�B
cTB
cTB
cTB
cTB
c�B
cnB
cnB
c�B
cnB
c�B
cnB
e`B
ezB
e`B
dtB
e`B
ezB
e�B
e�B
f�B
ffB
f�B
f�B
f�B
f�B
f�B
gmB
gmB
f�B
g�B
gmB
f�B
f�B
g�B
g�B
g�B
h�B
h�B
i�B
i�B
i�B
i�B
j�B
k�B
k�B
k�B
k�1111111111111111111111111111111111111111111144411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111131111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<0�|G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.08(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201712010033412017120100334120171201003341201806221234042018062212340420180622123404201804050430082018040504300820180405043008  JA  ARFMdecpA19c                                                                20171127093518  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20171127003528  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20171127003530  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20171127003530  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20171127003531  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20171127003531  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20171127003531  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20171127003531  QCF$                G�O�G�O�G�O�            4000JA  ARGQaqcp2.8e                                                                20171127003531  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20171127003531  QCF$                G�O�G�O�G�O�            4000JA  ARGQrqcpt16c                                                                20171127003531  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20171127003531                      G�O�G�O�G�O�                JA  ARUP                                                                        20171127005612                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20171127153356  CV  JULD            G�O�G�O�F�î                JM  ARSQJMQC2.0                                                                 20171129000000  CF  PSAL_ADJUSTED_QCD�� D�� G�O�                JM  ARCAJMQC2.0                                                                 20171130153341  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20171130153341  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404193008  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033404  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121517                      G�O�G�O�G�O�                