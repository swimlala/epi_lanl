CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-11-08T00:35:28Z creation;2016-11-08T00:35:30Z conversion to V3.1;2019-12-19T08:26:00Z update;     
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
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p@   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t(   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �P   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ݈   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20161108003528  20200115111518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               7A   JA  I2_0576_055                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @��t�� 1   @��uF)� @:�($x�d�qu�"1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @���A   A   A>ffA`  A���A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bw��B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8�fD9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�<�D�|�D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�C3Dڃ3D��3D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D���D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @z�H@�=q@�p�A�RA=�A^�RA�(�A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�BwG�B�B��
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
=B��
C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D	z�D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D#��D$z�D$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8�HD8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtz�Dt��Duz�Du��Dvz�Dv��Dwz�Dw��Dxz�Dx��Dyz�Dy��Dzz�Dz��D{z�D{��D|z�D|��D}z�D}��D~z�D~��Dz�D��D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD½qD��qD�=qD�}qDýqD��qD�=qD�}qDĽqD��qD�=qD�}qDŽqD��qD�=qD�}qDƽqD��qD�=qD�}qDǽqD��qD�=qD�}qDȽqD��qD�=qD�}qDɽqD��qD�=qD�}qDʽqD��qD�=qD�}qD˽qD��qD�=qD�}qD̽qD��qD�=qD�}qDͽqD��qD�=qD�}qDνqD��qD�=qD�}qDϽqD��qD�=qD�}qDнqD��qD�=qD�}qDѽqD��qD�=qD�}qDҽqD��qD�=qD�}qDӽqD��qD�=qD�}qDԽqD��qD�=qD�}qDսqD��qD�=qD�}qDֽqD��qD�:=D�z=D׽qD��qD�=qD�}qDؽqD��qD�=qD�}qDٽqD��qD�@�Dڀ�D���D��qD�=qD�}qD۽qD��qD�=qD�}qDܽqD��qD�=qD�}qDݽqD��=D�=qD�}qD޽qD��qD�=qD�}qD߽qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD�q111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�Q�A�VA�l�A�`BA�hsA�p�A�l�A�n�A�r�A�hsA�n�A�t�A�t�A�r�A�t�A�t�A�t�A�n�A�r�A�hsA�r�A�t�A�|�A�|�A�z�A�x�A�v�A�x�A�`BA���Aʣ�A�VA�E�A�M�A�x�A�1'A���A��!A�  A���A�^5A�Q�A���A�~�A�5?A��PA��A���A�?}A�ȴA��jA�;dA���A��#A� �A��A�hsA�C�A��;A��mA���A���A�;dA��uA���A�z�A��hA��HA���A��wA���A�5?A��-A�5?A��#A��A��-A��/A�t�A� �A�+A�bA�"�A�&�A��A���A�dZA�I�A�{A���A��+A�r�A���A���A�VA�I�A�jA��PA��\A�JA��^A�M�A��#A�S�A��A��HA�\)A���A��;A��-A�bA��+A��TA�1'A��-A���A�9XA�bNA�AS�A~ZAzz�Ax�DAw��Aw��Av�\AvI�Av-Au�At(�As�PAp�Aot�Am��AjĜAf��Ac�A^ffA[�TAZbAY+AWO�AU
=ATjAS/AQ��AP��AP9XAO\)AN�HAN��AM��AN9XAN(�AM�mAMp�ALĜAK�AK;dAJ1AH�AG\)AFjAE�AD��AD9XAB��A@�\A>��A>�A<=qA<�A;�
A;��A;K�A:ĜA:�A9��A8ZA7�A6 �A5"�A3�A1�A1+A0ffA/��A.��A-�wA,(�A*�\A)��A)VA(bA'�7A'p�A';dA&��A&��A&ffA&1'A%��A$�/A#�A"A�A!�A �A ��A v�A�wAt�A��A��A�;A�7AXA/A�A�A��A{A&�A9XA-A-A-A(�AG�A�\Av�Ar�AQ�A��A�A�mAz�A1'A|�An�Ap�Ar�Al�AM�AȴA�^A
ĜA��A33A9XA�wA/A�A�uAz�A�A��A?}A�A��A9XAt�A V@�|�@��+@�{@��@��@�7L@�ƨ@�v�@��@�E�@�$�@��@�l�@��y@��@�=q@�b@�-@���@���@�9@�33@�n�@��@�`B@��`@ܴ9@��@���@۝�@�\)@�
=@���@�V@�r�@׍P@�v�@Ձ@ӍP@�z�@�{@�j@˕�@���@Ɂ@ǝ�@�@�`B@��@ċD@ÍP@¸R@��u@�
=@��\@�S�@��@���@��9@�I�@�33@��D@�bN@��@�\)@�-@�V@� �@���@�7L@��
@��+@��@��
@�\)@���@�@�p�@�1@��!@�?}@���@�9X@�1@��@��
@��F@���@�|�@�o@�^5@��@�1'@��;@��
@���@�@�ȴ@��@�O�@�j@�  @��
@��m@��;@���@�@��@��^@��^@���@���@�p�@��@���@�S�@�S�@��y@�-@��-@��7@���@��;@�l�@�dZ@�;d@���@��+@�=q@�{@��#@���@���@��@�%@��@���@�`B@�/@���@�9X@���@���@��m@��m@��m@��m@��@�
=@��!@�^5@�$�@�{@�{@��@��-@�G�@��@��9@��@�bN@��@��
@�"�@�@���@�^5@�$�@��T@���@�p�@�?}@��D@��@��
@�|�@�@��@�ȴ@���@�-@��@��-@��@�hs@�/@���@��`@��j@���@�r�@�Z@�A�@�1'@�1@l�@~�+@~�+@~�y@~{@~@}�T@}?}@|�@|(�@{o@zn�@y��@yhs@x��@x �@w��@w�P@wK�@vV@u�@u��@u�h@u�@u?}@tz�@s�
@sdZ@s@r��@r��@r��@r^5@r=q@r=q@r=q@r�@q�@q�@q��@qX@p��@pr�@p1'@o��@oK�@o+@n�R@nv�@nV@nE�@n{@n@m�-@m`B@m`B@mp�@m�@mp�@m`B@m`B@mp�@m�@m�@m�h@m�h@m�h@m�@m�@l�@lI�@kdZ@jM�@j=q@i�#@h��@h�u@h  @g|�@g;d@g
=@f��@fff@f5?@f5?@f5?@f@e�@e�@d�@dj@d9X@cƨ@ct�@cS�@c"�@b�!@a��@a��@a��@a�7@_��@_�P@_K�@_
=@_
=@^�y@^ff@^5?@^{@]p�@\��@\�@\�j@\�D@\I�@\1@[�F@[dZ@Z�@Z�@Z�@Z��@Z=q@Y�@Y��@Y�^@Y��@Y��@Y�7@Yx�@Y��@Y�^@Yx�@YX@Y�@X��@X�9@XQ�@X  @W�w@W|�@WK�@W
=@V�R@V@U?}@T�@T��@T��@T�@Tj@T�@S��@S�m@SC�@S@R��@R=q@Qx�@Q&�@P�9@P�@O�@O��@O
=@N��@NV@M�@M@M/@M/@L�/@Lz�@K��@K��@KS�@J�@J=q@I��@I�7@Ihs@Ihs@IG�@I7L@H��@H1'@G��@G�w@G�@G��@Gl�@G\)@F��@F�+@F5?@E@E�@E`B@E/@D�/@D��@D9X@D1@C�@CS�@CdZ@Ct�@C�@Ct�@Ct�@CS�@C33@C33@C"�@B��@B~�@B^5@B-@A�#@A�#@A��@A�^@A��@A�7@Ahs@AG�@A&�@A%@@�9@@�@@  @?|�@>�@>��@>v�@=�T@=/@<�@<��@<j@<1@;��@;o@:�H@:n�@:�@9��@9�#@9��@9�^@9�7@97L@9%@8��@8�9@8��@8�u@8�@8r�@8Q�@81'@7�;@7\)@6��@6v�@65?@5�@5@5�h@5`B@5O�@5V@4�j@4��@4�D@4Z@4I�@4�@3�
@3�F@3��@3��@3��@3�@3"�@2��@2n�@2=q@2J@1��@1X@0�`@0�9@0bN@0 �@/��@/��@/l�@/;d@/;d@/+@/+@.�y@.��@.V@.5?@.5?@.$�@.@-�T@-��@-`B@-/@,�@,��@,9X@+ƨ@+dZ@+"�@+@*��@*�!@*n�@*�@)��@)��@)��@)X@)7L@(��@(�u@(Q�@(Q�@(1'@(  @'�@'�P@'|�@'K�@&ȴ@&v�@&V@&{@%��@%@%�@%/@%V@$�/@$z�@$9X@$(�@#�m@#"�@"�@"�H@"��@"��@"M�@"-@!��@ �`@  �@   @�;@�w@�w@�@�@�@�@�@��@K�@;d@;d@;d@+@$�@��@�h@p�@?}@�@�j@z�@I�@9X@(�@(�@�@��@�
@ƨ@��@t�@S�@C�@33@33@o@��@�!@~�@~�@n�@��@x�@hs@X@G�@&�@��@��@��@�@r�@1'@b@  @�;@�w@��@l�@�@��@�y@�y@�@�R@��@v�@ff@V@5?@@�-@�@��@�j@�@�D@9X@�m@�m@�m@�m@�
@�F@dZ@33@o@��@��@�\@�\@M�@=q@-@��@��@�7@X@�@�9@�u@r�@Q�@ �@b@  @�P@K�@ȴ@�+@v�@V@$�@{@{@{@{@@�@�j@I�@9X@�@1@1@��@��@�m@��@dZ@C�@"�@o@o@@
��@
��@
��@
M�@
J@	�#@	�^@	��@	x�@	x�@	7L@�`@��@r�@Q�@b@b@  @�;@�w@��@��@��@��@��@��@|�@;d@��@ȴ@��@v�@V@{@�h@`B@?}@��@�@�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�Q�A�VA�l�A�`BA�hsA�p�A�l�A�n�A�r�A�hsA�n�A�t�A�t�A�r�A�t�A�t�A�t�A�n�A�r�A�hsA�r�A�t�A�|�A�|�A�z�A�x�A�v�A�x�A�`BA���Aʣ�A�VA�E�A�M�A�x�A�1'A���A��!A�  A���A�^5A�Q�A���A�~�A�5?A��PA��A���A�?}A�ȴA��jA�;dA���A��#A� �A��A�hsA�C�A��;A��mA���A���A�;dA��uA���A�z�A��hA��HA���A��wA���A�5?A��-A�5?A��#A��A��-A��/A�t�A� �A�+A�bA�"�A�&�A��A���A�dZA�I�A�{A���A��+A�r�A���A���A�VA�I�A�jA��PA��\A�JA��^A�M�A��#A�S�A��A��HA�\)A���A��;A��-A�bA��+A��TA�1'A��-A���A�9XA�bNA�AS�A~ZAzz�Ax�DAw��Aw��Av�\AvI�Av-Au�At(�As�PAp�Aot�Am��AjĜAf��Ac�A^ffA[�TAZbAY+AWO�AU
=ATjAS/AQ��AP��AP9XAO\)AN�HAN��AM��AN9XAN(�AM�mAMp�ALĜAK�AK;dAJ1AH�AG\)AFjAE�AD��AD9XAB��A@�\A>��A>�A<=qA<�A;�
A;��A;K�A:ĜA:�A9��A8ZA7�A6 �A5"�A3�A1�A1+A0ffA/��A.��A-�wA,(�A*�\A)��A)VA(bA'�7A'p�A';dA&��A&��A&ffA&1'A%��A$�/A#�A"A�A!�A �A ��A v�A�wAt�A��A��A�;A�7AXA/A�A�A��A{A&�A9XA-A-A-A(�AG�A�\Av�Ar�AQ�A��A�A�mAz�A1'A|�An�Ap�Ar�Al�AM�AȴA�^A
ĜA��A33A9XA�wA/A�A�uAz�A�A��A?}A�A��A9XAt�A V@�|�@��+@�{@��@��@�7L@�ƨ@�v�@��@�E�@�$�@��@�l�@��y@��@�=q@�b@�-@���@���@�9@�33@�n�@��@�`B@��`@ܴ9@��@���@۝�@�\)@�
=@���@�V@�r�@׍P@�v�@Ձ@ӍP@�z�@�{@�j@˕�@���@Ɂ@ǝ�@�@�`B@��@ċD@ÍP@¸R@��u@�
=@��\@�S�@��@���@��9@�I�@�33@��D@�bN@��@�\)@�-@�V@� �@���@�7L@��
@��+@��@��
@�\)@���@�@�p�@�1@��!@�?}@���@�9X@�1@��@��
@��F@���@�|�@�o@�^5@��@�1'@��;@��
@���@�@�ȴ@��@�O�@�j@�  @��
@��m@��;@���@�@��@��^@��^@���@���@�p�@��@���@�S�@�S�@��y@�-@��-@��7@���@��;@�l�@�dZ@�;d@���@��+@�=q@�{@��#@���@���@��@�%@��@���@�`B@�/@���@�9X@���@���@��m@��m@��m@��m@��@�
=@��!@�^5@�$�@�{@�{@��@��-@�G�@��@��9@��@�bN@��@��
@�"�@�@���@�^5@�$�@��T@���@�p�@�?}@��D@��@��
@�|�@�@��@�ȴ@���@�-@��@��-@��@�hs@�/@���@��`@��j@���@�r�@�Z@�A�@�1'@�1@l�@~�+@~�+@~�y@~{@~@}�T@}?}@|�@|(�@{o@zn�@y��@yhs@x��@x �@w��@w�P@wK�@vV@u�@u��@u�h@u�@u?}@tz�@s�
@sdZ@s@r��@r��@r��@r^5@r=q@r=q@r=q@r�@q�@q�@q��@qX@p��@pr�@p1'@o��@oK�@o+@n�R@nv�@nV@nE�@n{@n@m�-@m`B@m`B@mp�@m�@mp�@m`B@m`B@mp�@m�@m�@m�h@m�h@m�h@m�@m�@l�@lI�@kdZ@jM�@j=q@i�#@h��@h�u@h  @g|�@g;d@g
=@f��@fff@f5?@f5?@f5?@f@e�@e�@d�@dj@d9X@cƨ@ct�@cS�@c"�@b�!@a��@a��@a��@a�7@_��@_�P@_K�@_
=@_
=@^�y@^ff@^5?@^{@]p�@\��@\�@\�j@\�D@\I�@\1@[�F@[dZ@Z�@Z�@Z�@Z��@Z=q@Y�@Y��@Y�^@Y��@Y��@Y�7@Yx�@Y��@Y�^@Yx�@YX@Y�@X��@X�9@XQ�@X  @W�w@W|�@WK�@W
=@V�R@V@U?}@T�@T��@T��@T�@Tj@T�@S��@S�m@SC�@S@R��@R=q@Qx�@Q&�@P�9@P�@O�@O��@O
=@N��@NV@M�@M@M/@M/@L�/@Lz�@K��@K��@KS�@J�@J=q@I��@I�7@Ihs@Ihs@IG�@I7L@H��@H1'@G��@G�w@G�@G��@Gl�@G\)@F��@F�+@F5?@E@E�@E`B@E/@D�/@D��@D9X@D1@C�@CS�@CdZ@Ct�@C�@Ct�@Ct�@CS�@C33@C33@C"�@B��@B~�@B^5@B-@A�#@A�#@A��@A�^@A��@A�7@Ahs@AG�@A&�@A%@@�9@@�@@  @?|�@>�@>��@>v�@=�T@=/@<�@<��@<j@<1@;��@;o@:�H@:n�@:�@9��@9�#@9��@9�^@9�7@97L@9%@8��@8�9@8��@8�u@8�@8r�@8Q�@81'@7�;@7\)@6��@6v�@65?@5�@5@5�h@5`B@5O�@5V@4�j@4��@4�D@4Z@4I�@4�@3�
@3�F@3��@3��@3��@3�@3"�@2��@2n�@2=q@2J@1��@1X@0�`@0�9@0bN@0 �@/��@/��@/l�@/;d@/;d@/+@/+@.�y@.��@.V@.5?@.5?@.$�@.@-�T@-��@-`B@-/@,�@,��@,9X@+ƨ@+dZ@+"�@+@*��@*�!@*n�@*�@)��@)��@)��@)X@)7L@(��@(�u@(Q�@(Q�@(1'@(  @'�@'�P@'|�@'K�@&ȴ@&v�@&V@&{@%��@%@%�@%/@%V@$�/@$z�@$9X@$(�@#�m@#"�@"�@"�H@"��@"��@"M�@"-@!��@ �`@  �@   @�;@�w@�w@�@�@�@�@�@��@K�@;d@;d@;d@+@$�@��@�h@p�@?}@�@�j@z�@I�@9X@(�@(�@�@��@�
@ƨ@��@t�@S�@C�@33@33@o@��@�!@~�@~�@n�@��@x�@hs@X@G�@&�@��@��@��@�@r�@1'@b@  @�;@�w@��@l�@�@��@�y@�y@�@�R@��@v�@ff@V@5?@@�-@�@��@�j@�@�D@9X@�m@�m@�m@�m@�
@�F@dZ@33@o@��@��@�\@�\@M�@=q@-@��@��@�7@X@�@�9@�u@r�@Q�@ �@b@  @�P@K�@ȴ@�+@v�@V@$�@{@{@{@{@@�@�j@I�@9X@�@1@1@��@��@�m@��@dZ@C�@"�@o@o@@
��@
��@
��@
M�@
J@	�#@	�^@	��@	x�@	x�@	7L@�`@��@r�@Q�@b@b@  @�;@�w@��@��@��@��@��@��@|�@;d@��@ȴ@��@v�@V@{@�h@`B@?}@��@�@�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�sB�B�B�B�B�B�B��B��B��B�BǮB�qB�XB�LB�FB�FB�B�B��B��B��B��B��B��B��B�JBz�BgmBQ�BK�BH�BE�BB�B:^B1'B&�B!�B�B
=B��B�B�B�fB�)B�jB��B��B��B��B�B�LB�qB�}B��B�B�BB�mB�sB�mB�`B�NB��BȴB�wB�'B��B��B�bB�JB�+B� Bx�Bp�BdZB\)BQ�BH�B8RB,B#�B�BB
�B
�)B
ȴB
�RB
�!B
��B
��B
�1B
u�B
o�B
k�B
e`B
aHB
`BB
ZB
S�B
N�B
B�B
/B
oB	�yB	��B	k�B	@�B	7LB	.B	-B	(�B	�B	�B	�B	JB	B	B	�B	"�B	6FB	6FB	F�B	S�B	S�B	P�B	M�B	H�B	E�B	=qB	8RB	0!B	+B	#�B	�B	�B	oB	+B��B��B��B��B��B�B�B�B�B�B�B�fB�NB�BB�B��B��B��B��BŢBB�jB�?B�-B�B�B��B��B��B��B��B��B��B��B��B��B��B�hB�\B�\B�VB�JB�DB�7B�+B�%B�B�B�B�B�B� B~�B|�Bw�Bw�Bw�Bw�Bv�Bv�Br�Bq�Bq�Bp�Bp�Bm�BjBgmBe`BdZB`BB\)BYBW
BS�BO�BJ�BH�BF�BA�B?}B>wB=qB;dB:^B9XB9XB8RB8RB7LB7LB7LB6FB1'B0!B/B.B-B)�B%�B%�B"�B"�B �B�B�B�B�B�B�B�B�B�B{B{BuBoBuBuBuBuB{B{B{B{BuB�B�B�B�B�B�B�B�B!�B"�B"�B#�B#�B&�B(�B(�B(�B)�B)�B'�B%�B$�B'�B)�B$�B%�B'�B&�B(�B!�B!�B!�B!�B#�B$�B&�B(�B,B/B1'B5?B7LB8RB:^B;dB<jB@�BD�BH�BI�BJ�BL�BM�BN�BO�BR�BT�B[#BdZBdZBhsBhsBk�Bp�Bs�Bt�Bu�B{�By�Bz�B|�B� B�B�B�B�+B�+B�+B�+B�+B�+B�=B�\B�VB�\B�hB�hB�{B�{B�{B��B��B��B��B��B��B��B��B��B��B�B�B�-B�FB�^B�^B��BBÖBŢBƨBǮBȴBȴBɺB��B��B�B�#B�5B�5B�5B�;B�HB�`B�sB�B�B�B�B�B��B��B��B��B	B	B	B	+B		7B	VB	oB	uB	�B	�B	�B	!�B	"�B	$�B	'�B	(�B	+B	,B	.B	0!B	2-B	49B	5?B	7LB	8RB	9XB	9XB	<jB	?}B	A�B	C�B	H�B	I�B	J�B	K�B	M�B	R�B	T�B	]/B	_;B	_;B	aHB	dZB	dZB	e`B	gmB	jB	k�B	k�B	l�B	m�B	n�B	o�B	t�B	v�B	w�B	x�B	z�B	z�B	z�B	{�B	|�B	|�B	|�B	}�B	~�B	� B	�B	�B	�%B	�1B	�7B	�DB	�JB	�PB	�\B	�oB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�3B	�FB	�FB	�LB	�RB	�jB	�qB	�}B	B	ĜB	ƨB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�)B	�/B	�HB	�NB	�HB	�HB	�NB	�NB	�TB	�ZB	�`B	�fB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
%B
+B
1B
1B
1B
	7B

=B
DB
DB
DB
JB
PB
VB
VB
\B
\B
bB
bB
bB
hB
hB
oB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
"�B
"�B
"�B
"�B
#�B
$�B
%�B
%�B
%�B
&�B
'�B
'�B
'�B
(�B
(�B
)�B
)�B
)�B
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
.B
.B
/B
0!B
0!B
0!B
1'B
1'B
1'B
2-B
2-B
2-B
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
6FB
6FB
6FB
6FB
7LB
7LB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
:^B
9XB
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
=qB
=qB
<jB
=qB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
@�B
@�B
A�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
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
L�B
M�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
R�B
S�B
S�B
S�B
T�B
T�B
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
YB
YB
ZB
YB
YB
ZB
ZB
ZB
ZB
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
]/B
]/B
]/B
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
_;B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
cTB
cTB
cTB
cTB
dZB
cTB
cTB
cTB
cTB
cTB
dZB
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
jB
jB
jB
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
l�B
l�B
l�B
m�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
o�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�2B�2B�	B�JB��B�)B��B��B�8B�MB�TB�tB��B��B��B�B��B�4B�B�rB��B�$B��B�B�8B� B��B�HB��B�7B�]B�(B}BiDBR�BMBJrBG�BEmB<PB2�B(
B#�BEB�B�BB�B��B�B޸B�(B��B�/B��B��B��B�LB��B�iB˒B�_B��B�
B�B�
B�B��BԕBʦB��B�MB��B��B�NB��B�fB�oBz�Br|Be�B^BTBK)B:B-�B&B�B�B
�CB
�!B
ʦB
��B
�AB
�B
��B
�XB
v�B
p;B
l�B
e�B
a�B
a�B
[qB
U�B
Q�B
D�B
1�B
SB	�B	� B	p�B	C�B	9rB	/�B	/iB	+�B	�B	5B	$B	�B	B	aB	B	#nB	6�B	6FB	F�B	TaB	T�B	RB	N�B	J	B	GEB	?cB	9�B	1�B	,�B	$�B	�B	�B	2B		B�]B��B�+B�%B�%B�nB�B�B�B�OB�=B��B�&B�BںB��B�B�B�B�_BĜB�BB�`B�hB�;B��B�DB�sB��B�LB�`B�ZB��B� B��B�IB��B��B��B�B�\B��B��B��B�1B��B�gB�aB�GB��B��B� B�4B}�Bw�Bw�BxBxRBw�Bw�Br�Bq�BrBq�BrGBo Bl=Bh>Bf�Be�Ba�B]�BZ�BX�BVBQ�BL~BKDBH�BB�B@OB?cB>B;�B:�B:B9�B8�B8�B7�B88B8�B7�B2-B0�B/�B/ B.�B+6B'B&�B$&B$tB!-BpB 'B BB�B]BCB�B�BB�BgB�B�B�B�B�B�B�B�B�B�B,B9B
BYBsB�BWB�BpB#B#�B#�B%B%FB'�B)�B)yB)�B*�B+B)_B'B%�B)�B*�B%`B&�B(�B'�B*eB"NB"NB"�B"�B$�B%�B(
B*0B-)B0!B2-B6B7�B8�B:�B<6B=�BA�BE�BIBJ	BKBMBNBOBPBS@BU�B[�Bd�Be`Bh�Bh�Bk�Bq'BtBu%Bv�B|�Bz*B{0B}B�OB�oB��B��B�_B�_B�EB�EB�_B��B��B�.B��B��B��B��B��B��B�2B�	B��B�B�;B�B�B�$B�DB�DB�DB�QB�B��B�`B��B��B��B�-B��BŢBƨB��B��B��B�	B�VB�gB�yB�WB�jB�jB�jBߊB�B�B��B��B��B��B�B�MB�	B�>B�<B�.B	;B	GB	SB	zB		�B	�B	�B	�B	�B	�B	B	!�B	# B	%B	(>B	)*B	+B	,WB	.cB	0UB	2|B	4TB	5�B	7fB	8lB	9�B	9�B	<�B	?�B	A�B	C�B	IB	I�B	J�B	L0B	N"B	S[B	U�B	]dB	_�B	_pB	a�B	d�B	d�B	e�B	g�B	j�B	k�B	k�B	l�B	m�B	n�B	o�B	uB	v�B	xB	y	B	z�B	z�B	z�B	|B	|�B	}B	}"B	~B	.B	�4B	�;B	�MB	�tB	�fB	��B	�xB	�~B	��B	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	��B	�*B	�KB	�IB	�[B	��B	��B	�`B	��B	��B	��B	��B	��B	ªB	ĶB	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	�:B	�B	�&B	�&B	�,B	�,B	�B	�$B	�sB	ؓB	�1B	�KB	�7B	�QB	�7B	�QB	�CB	�dB	�|B	�B	�bB	�bB	�B	�B	�nB	�tB	�zB	�B	�B	�B	�B	�B	�B	��B	�B	��B	�B	��B	�B	�B	�B	��B	��B	��B	��B	��B	�	B	�B	�B	�B	�(B	�B	�HB
 OB
UB
AB
GB
3B
3B
3B
SB
YB
YB
zB
KB
fB
fB
	�B

�B
�B
xB
�B
~B
�B
�B
�B
�B
vB
�B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
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
!�B
!�B
!�B
#B
#B
"�B
#B
#B
$B
%B
%�B
%�B
&B
'8B
(
B
(
B
($B
)*B
)*B
*0B
*0B
*KB
+B
+B
+B
,B
,"B
,=B
,=B
-)B
-CB
-CB
-B
.B
./B
./B
./B
.IB
.cB
.IB
/iB
0oB
0;B
0;B
1[B
1AB
1AB
2GB
2aB
2GB
3MB
3hB
3MB
3MB
3MB
4TB
4nB
4TB
5?B
5tB
5ZB
5�B
5tB
6`B
6`B
6`B
6`B
7�B
7�B
8lB
8lB
8�B
9rB
9�B
9�B
9�B
9rB
:^B
9�B
9�B
:xB
:xB
;�B
;dB
;B
;B
;B
;�B
<�B
<�B
=�B
=�B
<�B
=�B
>�B
>�B
>�B
>�B
>�B
?�B
?�B
?�B
@�B
@�B
A�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
H�B
H�B
IB
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
LB
MB
M�B
MB
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
NB
M�B
M�B
M�B
M�B
N"B
OB
O�B
O�B
O�B
O�B
QB
QB
RB
Q�B
RB
Q�B
Q�B
R B
R B
SB
SB
S&B
S&B
TB
S�B
TB
T,B
T,B
TB
SB
TB
T,B
T,B
UB
UB
VB
VB
VB
VB
W$B
W$B
W$B
W$B
W$B
XEB
X+B
XEB
XEB
X+B
X+B
Y1B
YKB
Z7B
Y1B
Y1B
ZB
Z7B
Z7B
Z7B
Z7B
ZQB
Z7B
[=B
[WB
\]B
\)B
\]B
\CB
\CB
]dB
]/B
]/B
]IB
]dB
]IB
]dB
]IB
^OB
^OB
^OB
^jB
^OB
_VB
_;B
_pB
_VB
_VB
_pB
_VB
`vB
`vB
a|B
abB
abB
a|B
a|B
abB
a�B
a|B
b�B
c�B
cTB
cnB
cnB
dZB
cTB
cTB
cnB
c�B
c�B
d�B
ezB
ezB
ezB
ezB
e`B
e`B
e`B
ezB
e�B
f�B
f�B
f�B
f�B
ffB
f�B
f�B
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
jB
jB
j�B
j�B
j�B
jB
jB
jB
jB
jB
j�B
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
n�B
n�B
o�B
o�B
o�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<*d�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.08(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201611120032432016111200324320161112003243201806221216322018062212163220180622121632201804050409272018040504092720180405040927  JA  ARFMdecpA19c                                                                20161108093506  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20161108003528  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20161108003529  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20161108003529  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20161108003530  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20161108003530  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20161108003530  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20161108003530  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20161108003530  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20161108003530                      G�O�G�O�G�O�                JA  ARUP                                                                        20161108012949                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20161108153757  CV  JULD            G�O�G�O�F�å                JM  ARCAJMQC2.0                                                                 20161111153243  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20161111153243  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404190927  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622031632  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115111518                      G�O�G�O�G�O�                