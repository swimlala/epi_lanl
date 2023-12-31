CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-12-03T00:35:27Z creation;2017-12-03T00:35:31Z conversion to V3.1;2019-12-19T07:55:13Z update;     
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
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I    PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `l   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �<   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ܘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �    HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �$   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �(Argo profile    3.1 1.2 19500101000000  20171203003527  20200115121517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_185                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�9��"" 1   @�9�\�$ @;�'�0�d]f�A�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@���@���A   A@  A^ffA~ffA�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBhffBpffBxffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	y�D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH�fDI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЃ3D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ Dԃ3D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D���D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�=q@�=q@�=qA�RA>�RA]�A}�A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B`{Bh{Bp{Bx{B�B��
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
=B�
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
B��
B��
B��
B��
B��
B��
B��
C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D	t{D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D#��D$z�D$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DH�HDH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtz�Dt��Duz�Du��Dvz�Dv��Dwz�Dw��Dxz�Dx��Dyz�Dy��Dzz�Dz��D{z�D{��D|z�D|��D}z�D}��D~z�D~��Dz�D��D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�:=D�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD���D��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�z=D��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD½qD��qD�=qD�}qDýqD��qD�=qD�}qDĽqD��qD�=qD�}qDŽqD��qD�=qD�}qDƽqD��qD�=qD�}qDǽqD��qD�=qD�}qDȽqD��qD�=qD�}qDɽqD��qD�=qD�}qDʽqD��qD�=qD�}qD˽qD��qD�=qD�}qD̽qD��qD�=qD�}qDͽqD��qD�=qD�}qDνqD��qD�=qD�}qDϽqD��qD�=qDЀ�DнqD��qD�=qD�}qDѽqD��qD�=qD�}qDҽqD��qD�=qD�}qDӽqD��qD�=qDԀ�DԽqD��qD�=qD�}qDսqD��qD�=qD�}qDֽqD��qD�=qD�}qD׽qD��qD�=qD�}qDؽqD��qD�=qD�}qDٽqD��=D�=qD�}qDڽqD��qD�=qD�}qD۽qD��qD�=qD�}qDܽqD��qD�=qD�}qDݽqD��qD�=qD�}qD޽qD��qD�=qD�}qD߽qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�:=D�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD퀤D��qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�@�D�z=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�bNA�ffA�dZA�dZA�bNA�ffA�n�A�p�A�p�A�p�A�r�A�p�A�n�A�l�A�hsA�^5A�=qA�"�A��TA���A�$�A��A��^A�r�A��^A�(�A��-A�r�A�=qA�&�A�$�A�$�A�&�A�&�A�(�A�(�A�&�A�$�A��A�1A���A���A�5?A�S�A�Q�A�{A���A�33A��A��A�hsA��A�l�A��DA�  A�ĜA�^5A��
A��A�E�A���A��hA�z�A�l�A�M�A��#A�A�A�oA�
=A�z�A��HA���A���A�1A��#A���A��A���A��A�7LA�G�A��A�;dA~ZA|��A|{AyK�Aw�^Av��Au�AtZAs�As�wAs�hAr�9Aq��Aq%Ao�;An�An1'Amt�Al�yAlVAj�DAj=qAj  AiS�Ah�+Ag��AfQ�AeAd�jAcVAbJAahsA`(�A_VA^z�A^ �A]��A]"�A\~�A[\)AY�7AW�TAWoAV��AU��AT�AS��AS��AS�-ASx�AR�ARr�AP�AO�wANȴAM��AL��ALZAK33AJ�yAJz�AI��AH�HAG��AF�uAE+ADffAC��AC�ACXAC"�AA��A@bA?;dA>~�A=�wA=C�A=
=A;��A;|�A;%A:E�A:{A9�A9%A8E�A7S�A6�A5��A5?}A4A�A3�^A3�PA2jA0�yA09XA/�;A//A.�9A.bNA-�A,�`A,�!A,z�A+A*��A*�DA*1A)C�A(��A(ZA'�A'/A&M�A%�#A$��A#+A"��A"Q�A"1'A"{A!��A!��A!/A ��A r�A I�A �A��A�PAG�A�A��At�A?}A�A�7A�9A��A-Ax�A��A5?A�mA�`A��A�9A�DAVAx�A-A|�A
=A�A�A
�jA	7LAI�A  A�wA��A�+A-A�7A�AE�A�A��Ar�A+@��w@���@�  @�t�@��H@���@�ƨ@���@���@��@�@�  @��`@ꟾ@��@�-@�-@�@�7L@�@�-@�7L@䛦@��m@�@�C�@⟾@��@��@��@��`@�(�@�
=@ٙ�@�  @�=q@�/@�j@Ӿw@�33@��H@�=q@ёh@мj@�Z@�+@���@�ƨ@�t�@ʏ\@��@Ə\@�J@�C�@���@���@��h@��@�Ĝ@��F@�$�@��u@�-@�j@��R@�Ĝ@��
@�;d@�-@��T@��-@�x�@�V@���@�bN@���@���@�;d@�@��H@��!@�n�@�J@���@�p�@��@��j@�I�@��m@���@�dZ@�33@�J@�z�@�l�@�V@��@�  @�t�@��@��R@�=q@���@��@��@���@��@��@�bN@�bN@�9X@��@�1@�  @���@��m@���@��P@�+@���@�v�@��T@�&�@�Z@�1@���@��y@�~�@�$�@���@�7L@�&�@��j@���@�t�@�l�@�l�@��@��y@���@��@��@���@��@�j@�I�@��m@��P@�33@��@�ff@�5?@��#@���@���@�?}@�%@�b@��@�bN@�A�@�Q�@�I�@�;d@�V@�ff@��\@�J@�?}@�/@��u@�I�@�ƨ@�+@��H@��!@��\@�~�@�E�@��7@�x�@�7L@���@��@��@���@��;@��P@�"�@��@���@�n�@�5?@���@��#@�@���@�hs@���@���@�z�@�r�@�1'@�P@
=@
=@~�y@~�R@~$�@}�-@|z�@|1@{C�@{@z��@z��@zn�@zJ@y7L@xĜ@xA�@x �@w�@w��@w��@w�@x  @w�@vV@u��@u�h@u�@u�@up�@uO�@tz�@r�!@q��@q��@q��@q&�@pr�@pb@p �@o�w@o�P@o�P@o�P@n�+@m@m/@m?}@l�/@k��@k�@kdZ@kS�@ko@j��@jn�@i��@i��@i��@i��@i�7@i�@hbN@h�u@hQ�@h  @g�@h�u@ihs@i��@jn�@h��@f�y@f5?@e�-@e�T@f��@fȴ@fff@e@e�@dz�@c��@c�F@c��@cS�@b=q@a�^@aG�@`�`@`�9@`r�@`A�@`r�@a��@bn�@b^5@_�P@_�@_�@_�@_�@_�P@_K�@^�@^{@]�-@\��@\Z@[�m@[�
@[��@\1@\1@[�
@[C�@[@[C�@[t�@[t�@[dZ@[o@Z��@Y��@Y�#@Y��@Y&�@X�u@XA�@X  @W�P@W
=@VE�@TZ@SS�@S"�@S@R��@R^5@R�@Q�#@Q��@Q�7@Qhs@P��@P�u@O�w@N�R@NE�@M�-@M?}@L�@L��@Lz�@LZ@LZ@LI�@L9X@L9X@L9X@L(�@L(�@L(�@L1@K�m@K�m@K�
@K�F@K��@K�@KS�@K@J�!@J^5@JJ@I�@I��@I&�@H�@HQ�@H1'@H1'@Hb@G|�@F�@F�+@FV@F@E@E@E�-@E�-@E��@EO�@E?}@EV@D�j@Dz�@DZ@DI�@D(�@C�
@Cƨ@CdZ@C@B�H@B�\@A��@Ax�@AG�@A7L@@��@@A�@?\)@>�y@>v�@=��@=�@<�@<��@<�@<��@<j@<(�@;�m@;t�@:��@:n�@:^5@:-@9��@9��@9x�@9X@9�@8��@9�@9X@97L@8��@8r�@8Q�@8 �@7�;@7��@7|�@7K�@7�@7
=@6��@6{@5�h@5`B@5`B@5V@4�/@4j@49X@3��@3�
@3�F@3�@3t�@3S�@3o@2�\@2J@1��@1hs@1%@0��@0�`@0�`@0�`@0r�@0  @/��@/�@/��@/|�@/\)@/;d@/�@/�@/�@.��@.�@.��@.{@-�T@-��@-?}@,��@,�j@,(�@+t�@+o@*n�@)�7@)hs@)G�@)&�@(Ĝ@(Q�@(bN@(Q�@(Q�@(Q�@(A�@(1'@(b@'��@'K�@&��@&5?@%�-@%��@%�@%`B@%�@$�@$j@$1@#��@#�m@#�
@#��@#t�@#S�@#S�@#S�@#C�@#C�@#33@#"�@#@#@"�H@"�!@"��@"�\@"M�@!��@!�@!��@!��@!X@!X@!hs@!hs@!&�@!�@  �@�P@�@�R@��@�+@ff@ff@E�@$�@@��@@@�-@�h@O�@�@V@�/@��@�@Z@�m@�
@ƨ@�F@��@��@��@��@�@dZ@C�@o@@�@�!@~�@M�@-@J@��@�@�#@��@x�@7L@%@��@Ĝ@��@Q�@  @�@��@��@\)@;d@
=@ȴ@ȴ@�R@��@v�@5?@{@@@�@O�@V@��@��@�@�@�j@�j@�@j@�m@��@��@dZ@S�@C�@33@33@"�@"�@��@��@�\@^5@M�@=q@�@�^@�7@�7@�7@X@%@�`@��@Ĝ@�u@bN@A�@ �@|�@K�@;d@+@
=@�@��@V@$�@@��@�-@�-@�@O�@V@��@�@�@�@��@�j@��@z�@j@(�@1@��@ƨ@��@t�@@
��@
��@
M�@
=q@
-@	�@	�@	�#@	��@	x�@	X@	G�@	7L@	%@��@��@�u@�u@�u@�u@r�@b@�;@�w@�w@�@l�@��@��@v�@E�@5?@$�@{@@�@@`B@�@�@j@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�bNA�ffA�dZA�dZA�bNA�ffA�n�A�p�A�p�A�p�A�r�A�p�A�n�A�l�A�hsA�^5A�=qA�"�A��TA���A�$�A��A��^A�r�A��^A�(�A��-A�r�A�=qA�&�A�$�A�$�A�&�A�&�A�(�A�(�A�&�A�$�A��A�1A���A���A�5?A�S�A�Q�A�{A���A�33A��A��A�hsA��A�l�A��DA�  A�ĜA�^5A��
A��A�E�A���A��hA�z�A�l�A�M�A��#A�A�A�oA�
=A�z�A��HA���A���A�1A��#A���A��A���A��A�7LA�G�A��A�;dA~ZA|��A|{AyK�Aw�^Av��Au�AtZAs�As�wAs�hAr�9Aq��Aq%Ao�;An�An1'Amt�Al�yAlVAj�DAj=qAj  AiS�Ah�+Ag��AfQ�AeAd�jAcVAbJAahsA`(�A_VA^z�A^ �A]��A]"�A\~�A[\)AY�7AW�TAWoAV��AU��AT�AS��AS��AS�-ASx�AR�ARr�AP�AO�wANȴAM��AL��ALZAK33AJ�yAJz�AI��AH�HAG��AF�uAE+ADffAC��AC�ACXAC"�AA��A@bA?;dA>~�A=�wA=C�A=
=A;��A;|�A;%A:E�A:{A9�A9%A8E�A7S�A6�A5��A5?}A4A�A3�^A3�PA2jA0�yA09XA/�;A//A.�9A.bNA-�A,�`A,�!A,z�A+A*��A*�DA*1A)C�A(��A(ZA'�A'/A&M�A%�#A$��A#+A"��A"Q�A"1'A"{A!��A!��A!/A ��A r�A I�A �A��A�PAG�A�A��At�A?}A�A�7A�9A��A-Ax�A��A5?A�mA�`A��A�9A�DAVAx�A-A|�A
=A�A�A
�jA	7LAI�A  A�wA��A�+A-A�7A�AE�A�A��Ar�A+@��w@���@�  @�t�@��H@���@�ƨ@���@���@��@�@�  @��`@ꟾ@��@�-@�-@�@�7L@�@�-@�7L@䛦@��m@�@�C�@⟾@��@��@��@��`@�(�@�
=@ٙ�@�  @�=q@�/@�j@Ӿw@�33@��H@�=q@ёh@мj@�Z@�+@���@�ƨ@�t�@ʏ\@��@Ə\@�J@�C�@���@���@��h@��@�Ĝ@��F@�$�@��u@�-@�j@��R@�Ĝ@��
@�;d@�-@��T@��-@�x�@�V@���@�bN@���@���@�;d@�@��H@��!@�n�@�J@���@�p�@��@��j@�I�@��m@���@�dZ@�33@�J@�z�@�l�@�V@��@�  @�t�@��@��R@�=q@���@��@��@���@��@��@�bN@�bN@�9X@��@�1@�  @���@��m@���@��P@�+@���@�v�@��T@�&�@�Z@�1@���@��y@�~�@�$�@���@�7L@�&�@��j@���@�t�@�l�@�l�@��@��y@���@��@��@���@��@�j@�I�@��m@��P@�33@��@�ff@�5?@��#@���@���@�?}@�%@�b@��@�bN@�A�@�Q�@�I�@�;d@�V@�ff@��\@�J@�?}@�/@��u@�I�@�ƨ@�+@��H@��!@��\@�~�@�E�@��7@�x�@�7L@���@��@��@���@��;@��P@�"�@��@���@�n�@�5?@���@��#@�@���@�hs@���@���@�z�@�r�@�1'@�P@
=@
=@~�y@~�R@~$�@}�-@|z�@|1@{C�@{@z��@z��@zn�@zJ@y7L@xĜ@xA�@x �@w�@w��@w��@w�@x  @w�@vV@u��@u�h@u�@u�@up�@uO�@tz�@r�!@q��@q��@q��@q&�@pr�@pb@p �@o�w@o�P@o�P@o�P@n�+@m@m/@m?}@l�/@k��@k�@kdZ@kS�@ko@j��@jn�@i��@i��@i��@i��@i�7@i�@hbN@h�u@hQ�@h  @g�@h�u@ihs@i��@jn�@h��@f�y@f5?@e�-@e�T@f��@fȴ@fff@e@e�@dz�@c��@c�F@c��@cS�@b=q@a�^@aG�@`�`@`�9@`r�@`A�@`r�@a��@bn�@b^5@_�P@_�@_�@_�@_�@_�P@_K�@^�@^{@]�-@\��@\Z@[�m@[�
@[��@\1@\1@[�
@[C�@[@[C�@[t�@[t�@[dZ@[o@Z��@Y��@Y�#@Y��@Y&�@X�u@XA�@X  @W�P@W
=@VE�@TZ@SS�@S"�@S@R��@R^5@R�@Q�#@Q��@Q�7@Qhs@P��@P�u@O�w@N�R@NE�@M�-@M?}@L�@L��@Lz�@LZ@LZ@LI�@L9X@L9X@L9X@L(�@L(�@L(�@L1@K�m@K�m@K�
@K�F@K��@K�@KS�@K@J�!@J^5@JJ@I�@I��@I&�@H�@HQ�@H1'@H1'@Hb@G|�@F�@F�+@FV@F@E@E@E�-@E�-@E��@EO�@E?}@EV@D�j@Dz�@DZ@DI�@D(�@C�
@Cƨ@CdZ@C@B�H@B�\@A��@Ax�@AG�@A7L@@��@@A�@?\)@>�y@>v�@=��@=�@<�@<��@<�@<��@<j@<(�@;�m@;t�@:��@:n�@:^5@:-@9��@9��@9x�@9X@9�@8��@9�@9X@97L@8��@8r�@8Q�@8 �@7�;@7��@7|�@7K�@7�@7
=@6��@6{@5�h@5`B@5`B@5V@4�/@4j@49X@3��@3�
@3�F@3�@3t�@3S�@3o@2�\@2J@1��@1hs@1%@0��@0�`@0�`@0�`@0r�@0  @/��@/�@/��@/|�@/\)@/;d@/�@/�@/�@.��@.�@.��@.{@-�T@-��@-?}@,��@,�j@,(�@+t�@+o@*n�@)�7@)hs@)G�@)&�@(Ĝ@(Q�@(bN@(Q�@(Q�@(Q�@(A�@(1'@(b@'��@'K�@&��@&5?@%�-@%��@%�@%`B@%�@$�@$j@$1@#��@#�m@#�
@#��@#t�@#S�@#S�@#S�@#C�@#C�@#33@#"�@#@#@"�H@"�!@"��@"�\@"M�@!��@!�@!��@!��@!X@!X@!hs@!hs@!&�@!�@  �@�P@�@�R@��@�+@ff@ff@E�@$�@@��@@@�-@�h@O�@�@V@�/@��@�@Z@�m@�
@ƨ@�F@��@��@��@��@�@dZ@C�@o@@�@�!@~�@M�@-@J@��@�@�#@��@x�@7L@%@��@Ĝ@��@Q�@  @�@��@��@\)@;d@
=@ȴ@ȴ@�R@��@v�@5?@{@@@�@O�@V@��@��@�@�@�j@�j@�@j@�m@��@��@dZ@S�@C�@33@33@"�@"�@��@��@�\@^5@M�@=q@�@�^@�7@�7@�7@X@%@�`@��@Ĝ@�u@bN@A�@ �@|�@K�@;d@+@
=@�@��@V@$�@@��@�-@�-@�@O�@V@��@�@�@�@��@�j@��@z�@j@(�@1@��@ƨ@��@t�@@
��@
��@
M�@
=q@
-@	�@	�@	�#@	��@	x�@	X@	G�@	7L@	%@��@��@�u@�u@�u@�u@r�@b@�;@�w@�w@�@l�@��@��@v�@E�@5?@$�@{@@�@@`B@�@�@j@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B)�B(�B(�B)�B)�B)�B)�B(�B(�B(�B(�B(�B(�B'�B&�B$�B#�B �B�B�BuBbB
=BuB�B�B�B�B�B�B�B�B �B�B�B�B�B�B�B�B�BDB�HB��B"�B�dB�?B�jB�3B��B�VB}�Br�BcTB\)BN�BE�B7LB$�B�B"�B!�B �B�B�BhB  B
�B
�B
�mB
�TB
�B
��B
��B
��B
ŢB
�qB
�jB
�FB
�B
��B
��B
�JB
~�B
y�B
hsB
`BB
[#B
T�B
J�B
I�B
H�B
E�B
@�B
9XB
49B
-B
&�B
!�B
�B
�B
{B
	7B
	7B
1B
B	��B	��B	�B	�yB	�TB	�
B	��B	��B	ǮB	��B	��B	�qB	�^B	�9B	�B	��B	��B	�uB	�oB	�bB	�=B	�B	�B	�B	�B	� B	z�B	v�B	n�B	ffB	cTB	]/B	[#B	W
B	R�B	R�B	O�B	K�B	E�B	=qB	9XB	1'B	2-B	1'B	.B	-B	)�B	!�B	�B	�B	�B	oB	bB	VB		7B	B	B��B��B��B��B��B�B�B�B�B�ZB�TB�NB�B��B��B��B��B��B��BȴBŢBƨBŢB�}B�jB�^B�RB�FB�FB�?B�-B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�hB�JB�Bq�Bn�Bt�Bx�B|�Bz�Bu�Bs�Bt�Bs�BjBe`BffBn�Bk�BbNB^5B`BBbNBZB\)BXBP�BVB[#B[#BW
BZBYBVBO�BK�BI�BI�BJ�BB�BB�BA�BF�BH�BG�BA�B@�B@�B9XB7LB;dB:^B5?B8RB?}BC�BC�BA�B>wB8RB49B=qB=qB<jB>wB<jB:^B:^B8RB1'B'�B49B2-B1'B0!B/B2-B5?B7LB7LB8RB7LB8RB8RB:^B7LB6FB9XB=qB9XB6FB5?B;dB1'B1'B8RB=qBA�BA�B>wB>wB=qB=qBB�BE�BE�BK�BO�BQ�BXBXBYBZB]/B]/B]/B^5B^5BdZBgmBhsBiyBjBn�Bo�Bq�Br�Bt�Bv�Bw�Bx�Bw�Bu�Bv�By�B{�B�B�B�1B�=B�DB�JB�PB�PB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�!B�B�B�9B�?B�?B�9B�FB�RB�^B�wB��B��B�
B�B�#B�TB�yB�B�B�B�B��B��B�B��B�B��B��B	B	B	B	B	1B	JB	\B	VB	PB	bB	\B	oB	bB	\B	hB	{B	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	#�B	%�B	(�B	,B	-B	-B	.B	.B	1'B	49B	5?B	49B	5?B	6FB	9XB	;dB	<jB	<jB	<jB	<jB	C�B	F�B	K�B	L�B	M�B	M�B	M�B	N�B	Q�B	S�B	XB	YB	YB	ZB	[#B	]/B	`BB	`BB	e`B	jB	k�B	l�B	k�B	k�B	iyB	hsB	k�B	n�B	n�B	m�B	p�B	s�B	u�B	u�B	v�B	v�B	w�B	v�B	y�B	}�B	�B	�B	�B	�B	�7B	�=B	�DB	�DB	�JB	�PB	�hB	�oB	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�-B	�-B	�-B	�-B	�9B	�FB	�RB	�XB	�RB	�RB	�jB	�wB	��B	��B	B	ÖB	ƨB	ɺB	��B	��B	��B	��B	��B	�B	�
B	�B	�)B	�#B	�B	�B	�)B	�)B	�)B	�)B	�/B	�/B	�5B	�;B	�BB	�HB	�`B	�fB	�mB	�sB	�mB	�mB	�mB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B	��B
  B
B
B
B
B
B
B
B
B
B
%B
1B
1B
1B
	7B
	7B

=B

=B

=B
DB
DB
DB
DB
DB
JB
DB
JB
JB
JB
JB
\B
\B
\B
VB
VB
\B
bB
hB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
#�B
"�B
#�B
"�B
$�B
$�B
$�B
%�B
%�B
&�B
%�B
%�B
%�B
&�B
'�B
'�B
)�B
+B
,B
+B
+B
)�B
+B
-B
-B
.B
.B
.B
/B
/B
/B
/B
/B
/B
/B
/B
1'B
1'B
1'B
2-B
2-B
1'B
1'B
2-B
2-B
2-B
49B
49B
49B
49B
5?B
8RB
8RB
8RB
8RB
8RB
8RB
7LB
7LB
8RB
7LB
8RB
8RB
9XB
:^B
;dB
;dB
<jB
<jB
=qB
>wB
>wB
>wB
>wB
?}B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
A�B
A�B
A�B
?}B
@�B
@�B
B�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
I�B
I�B
I�B
J�B
J�B
J�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
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
O�B
P�B
P�B
O�B
O�B
P�B
P�B
P�B
P�B
N�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
Q�B
P�B
P�B
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
R�B
S�B
T�B
T�B
T�B
VB
T�B
T�B
W
B
XB
XB
XB
XB
YB
ZB
ZB
YB
ZB
ZB
ZB
YB
ZB
[#B
[#B
[#B
\)B
[#B
\)B
\)B
]/B
]/B
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
_;B
_;B
_;B
`BB
`BB
`BB
`BB
aHB
`BB
aHB
bNB
bNB
cTB
cTB
bNB
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
e`B
e`B
e`B
dZB
dZB
e`B
e`B
ffB
e`B
e`B
e`B
e`B
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
iyB
j111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B)�B)B(�B)�B)�B)�B)�B(�B(�B)B)B(�B(�B(
B'B%,B$&B!bBjB�BBTB�B�B�B�B)BB�B�B�B�B �B�B�B�B�B�B�BB�B�B�eB�LB1�B�)B�B��B��B�'B��B��Bv�BgB_!BQ�BG_B:*B'�BB#B!�B!BVBB@B�B
��B
��B
��B
�ZB
�7B
�B
�\B
˒B
��B
��B
��B
�fB
�/B
� B
�WB
��B
��B
{dB
k�B
a�B
\CB
VSB
LdB
J=B
IB
F%B
A�B
:^B
5tB
.}B
($B
"�B
�B
eB
�B
B
	�B
�B
�B	�B	��B	�;B	�B	��B	��B	�FB	� B	�RB	��B	�UB	��B	��B	�%B	�OB	��B	��B	�MB	�uB	� B	��B	�tB	��B	�SB	�aB	��B	{�B	w�B	poB	h
B	d�B	^�B	\)B	XB	T,B	SuB	P�B	L�B	GB	?B	:�B	2�B	33B	1�B	.�B	-wB	*�B	#�B	�B	�B	�B	[B	B	�B	
�B	�B	�B��B�cB��B��B��B�B��B�WB�6B�B�B�B��BөB��BөB��B̈́B�jBɺBƨB�B�%B��B�qB�B�$B�LB��B��B��B�IB�B��B�ZB��B�|B�4B�B� B�4B�VB�IB�WB��B��B�#B�1B��B�B� B�6B��Bt�BqvBv`By�B}<B{�Bv�Bt�ButBt�Bl"BgBg�Bn�Bl"Bc�B_�BaHBc B[�B]IBYeBR�BW$B[�B[�BXEBZ�BY�BW
BP�BL�BJ�BJ�BK�BD�BDgBCBG�BI7BHfBB�BAoBA�B:�B8�B<jB<B7LB9�B@ BC�BC�BA�B>�B9�B5�B=�B=�B<�B>�B<�B:�B:�B9	B2�B)�B4�B3B2GB1[B0oB3B5�B7�B7�B8�B7�B8�B9$B:�B8lB7�B:DB=�B:DB7�B6�B<PB3MB2�B9�B>wBA�BBB?}B?�B>�B?.BC�BF�BGBL�BPbBR�BX_BX_BYBZ�B]�B]�B]�B^�B^�Bd�Bg�Bh�Bi�Bj�Bn�BpBq�Bs3Bu%BwBxBy>Bx8Bv�Bw�Bz�B|�B��B�9B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�5B�5B�bB�TB�8B�XB�yB�qB�wB�wB��B�UB��B��B�nB�tB�tB��B�zB��B��B�B��B�B�$B�KBیB�B��B��B��B��B�B��B�B�B�%B�hB��B��B	MB	9B	�B	�B	�B	JB	\B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	$B	�B	�B		B		B	B	�B	�B	B	B	 �B	"B	$&B	&B	)DB	,"B	-)B	-CB	.IB	.}B	1�B	4TB	5tB	4�B	5�B	6zB	9�B	;B	<�B	<�B	<�B	<�B	C�B	F�B	K�B	L�B	M�B	M�B	NB	OBB	R B	T,B	X+B	Y1B	YKB	ZB	[#B	]IB	`vB	`�B	e�B	jB	k�B	l�B	k�B	k�B	i�B	iB	k�B	n�B	n�B	m�B	p�B	s�B	u�B	u�B	v�B	v�B	xB	wLB	zDB	~(B	�B	�;B	�uB	�SB	�RB	�XB	�xB	�xB	��B	��B	�hB	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�6B	�0B	��B	�B	�-B	�aB	�|B	�|B	��B	�zB	�lB	��B	��B	��B	��B	��B	��B	��B	ªB	ðB	ƎB	ɆB	͹B	�.B	͟B	�B	��B	�B	�
B	�1B	�CB	�qB	�kB	�QB	ܒB	�xB	�CB	�)B	�/B	�/B	�jB	�pB	�vB	�|B	�`B	�B	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B	��B	�B	��B	��B	�B	��B	�B	��B	�B	��B	��B	�B	�B	��B	�B	��B	��B	�	B	�	B	�B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�"B	�B	�B
 B
 B	�.B
 OB
'B
-B
-B
GB
aB
GB
3B
SB
9B
?B
KB
1B
KB
	lB
	RB

XB

rB

rB
xB
^B
xB
^B
^B
dB
xB
~B
dB
~B
�B
�B
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 B
 B
 �B
"�B
#�B
# B
#�B
# B
%B
$�B
$�B
%�B
%�B
'B
&B
&B
&B
'8B
(
B
(>B
*0B
+B
,"B
+B
+B
*0B
+QB
-)B
-)B
./B
./B
./B
/5B
/OB
/5B
/B
/5B
/5B
/OB
/OB
1AB
1AB
1[B
2aB
2aB
1vB
1�B
2|B
2�B
2�B
4TB
4nB
4nB
4nB
5tB
8RB
8RB
8RB
8RB
8lB
8lB
7fB
7�B
8�B
7�B
8�B
8�B
9�B
:xB
;�B
;B
<�B
<�B
=�B
>�B
>wB
>�B
>�B
?�B
?�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
A�B
A�B
A�B
?�B
@�B
@�B
B�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
I�B
I�B
I�B
J�B
J�B
J�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
MB
MB
MB
M�B
M�B
NB
M�B
NB
N�B
N�B
OB
OB
N�B
OB
OB
O�B
P�B
P�B
O�B
O�B
QB
Q B
P�B
Q B
OB
QB
Q�B
RB
RB
Q�B
RB
R�B
RB
QB
QB
R B
R�B
SB
R�B
SB
R�B
S�B
S�B
TB
SB
TB
T�B
U2B
UB
VB
U2B
UB
W?B
XB
X+B
X+B
X+B
YKB
Z7B
ZB
Y1B
Z7B
Z7B
Z7B
YeB
ZQB
[=B
[=B
[WB
\]B
[=B
\CB
\CB
]IB
]dB
^OB
^OB
^jB
^jB
^jB
_;B
_VB
_;B
_;B
_VB
_VB
_VB
_pB
_pB
_VB
`BB
`\B
`\B
`\B
a|B
`vB
abB
b�B
bhB
cTB
cnB
b�B
cTB
cTB
c�B
cnB
cnB
dZB
d�B
dtB
dtB
dtB
dtB
e`B
e`B
ezB
dtB
dtB
ezB
ezB
f�B
e�B
ezB
e�B
ezB
g�B
g�B
gmB
g�B
gmB
gmB
g�B
g�B
g�B
g�B
g�B
h�B
i�B
j111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<[��<m�h<|PH<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.08(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201712070033042017120700330420171207003304201806221234212018062212342120180622123421201804050430272018040504302720180405043027  JA  ARFMdecpA19c                                                                20171203093517  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20171203003527  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20171203003529  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20171203003530  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20171203003530  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20171203003530  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20171203003530  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20171203003530  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20171203003530  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20171203003531                      G�O�G�O�G�O�                JA  ARUP                                                                        20171203005504                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20171204153602  CV  JULD            G�O�G�O�F�Ϯ                JM  ARCAJMQC2.0                                                                 20171206153304  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20171206153304  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404193027  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033421  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121517                      G�O�G�O�G�O�                