CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-09-06T18:37:35Z creation;2016-09-06T18:37:37Z conversion to V3.1;2019-12-19T08:31:11Z update;     
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
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pL   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t4   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ݬ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �0   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �4   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �8   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �<Argo profile    3.1 1.2 19500101000000  20160906183735  20200115111518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               "A   JA  I2_0576_034                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�ȳ���1   @�ȴ:� @;9�"��`�dh�x���1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�  A   A!��A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C�fC  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,�C.  C0  C2  C4  C6  C8  C:  C<  C>�C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ D�|�D�� D�  D�@ Dր D�� D�  D�C3D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@4z�@z�H@�p�@�p�A Q�A>�RA^�RA~�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B��
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
B��
B��
B��
B��
B��
B��
B��
C�C�C�C�C	�C�C��C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C,C-�C/�C1�C3�C5�C7�C9�C;�C>C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D	z�D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D#��D$z�D$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtz�Dt��Duz�Du��Dvz�Dv��Dwz�Dw��Dxz�Dx��Dyz�Dy��Dzz�Dz��D{z�D{��D|z�D|��D}z�D}��D~z�D~��Dz�D��D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD� �D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��=D��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD½qD��qD�=qD�}qDýqD��qD�=qD�}qDĽqD��qD�=qD�}qDŽqD��qD�=qD�}qDƽqD��qD�=qD�}qDǽqD��qD�=qD�}qDȽqD��qD�=qD�}qDɽqD��qD�=qD�}qDʽqD��qD�=qD�}qD˽qD��qD�=qD�}qD̽qD��qD�=qD�}qDͽqD��qD�=qD�}qDνqD��qD�=qD�}qDϽqD��qD�=qD�}qDнqD��qD�=qD�}qDѽqD��qD�=qD�}qDҽqD��qD�=qD�}qDӽqD��qD�=qD�}qDԽqD��qD�=qD�z=DսqD��qD�=qD�}qDֽqD��qD�@�D�}qD׽qD��qD�=qD�}qDؽqD��qD�=qD�}qDٽqD��qD�=qD�}qDڽqD��qD�=qD�}qD۽qD��qD�=qD�}qDܽqD��qD�=qD�}qDݽqD��qD�=qD�}qD޽qD��qD�=qD�}qD߽qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�:=D�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD���D��qD��qD�=qD�}qD��qD��qD�=qD�}qD���D�q1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�E�A�?}A�5?A�1'A�1'A�1'A�1'A�1'A�1'A�1'A�/A�/A�1'A�1'A�33A�33A�33A�33A�5?A�5?A�5?A��#A�"�A�dZA�(�A�A�A��HA��A��
A��A�VA��A��TA�~�A���A��jA�S�A��yA��A�ffA�G�A��HA�z�A�ĜA�;dA���A��A��A�;dA�p�A�9XA�?}A���A�n�A�`BA��`A�&�A��7A�(�A�ƨA�dZA�bA��PA�bNA��RA�hsA�5?A�%A���A�VA���A�dZA�ZA�?}A���A��!A���A�$�A�JA��A�A��\A��A�=qA~I�A}�7A|�A|{Az�HAy��Aw33Avr�AvbNAvbAuK�At�DAs�
ArffAo�TAm�hAk�FAj��Ai�FAi/Ah�uAgt�AgG�Ag?}Ag"�Af�Ae|�AcƨAa�PA_��A]&�A[dZAZ�!AZ~�AZJAY%AW��AVE�AT�HAS�AQ�wAO�AO7LAM�^AM?}AMAL�HALȴALbNAL  AK/AKVAJ�/AJI�AI\)AH��AH�AHI�AG�-AG�AG7LAF��AE�TAE;dAD�AB��ABjABQ�AB=qAA�TA@�/A@E�A?33A>r�A> �A=��A;��A9��A9l�A8�A8bA7A7�7A7S�A6��A6bNA5��A4�A3
=A1"�A/��A/`BA/�A.��A,�`A+�hA*��A*ffA*9XA)�hA(��A(�A'�PA&^5A%�A"�A!�A!;dA �A�A&�A�`AȴA�9A��A�+AE�A�wAn�AXA��A��Ar�AƨA/AQ�AƨAS�A��A��A�-AZA9XA�-AS�A��A�
A�jAn�A
ĜA(�A\)A��AQ�A�AȴA^5A�#A��A�A ĜA A�@��R@�
=@��@��@��y@���@�&�@��@��y@�?}@��-@땁@��@�-@�V@�ƨ@�t�@�"�@��@�!@噚@��@�A�@�S�@��y@�V@ܴ9@�1@��y@٩�@أ�@�b@�-@�(�@�l�@Ұ!@�=q@���@ёh@�V@�Ĝ@�r�@�Q�@��;@��@�`B@�;d@ʧ�@�M�@���@�&�@�@��@���@��@��#@�O�@���@��@���@��!@�K�@���@��P@���@��/@��u@��u@�z�@�(�@��@��F@�l�@��y@��+@���@�/@�Ĝ@�r�@�(�@�=q@�V@�Ĝ@��@�A�@���@�@���@�V@�p�@��@��u@��@�j@��m@�\)@���@�M�@�ƨ@��@���@�E�@�G�@��@�j@���@�ȴ@�M�@���@��7@�7L@�z�@��@�ƨ@��@��P@�K�@�o@��R@�@���@�X@�33@�V@�E�@�$�@��h@�`B@��7@��@�r�@�  @�dZ@��\@�$�@�J@��@���@�?}@��/@�1'@���@��@�S�@�+@�@��!@���@�/@��9@�A�@�9X@�9X@�  @��
@���@�K�@��@��y@��\@�{@���@��j@��@��\@�v�@��@���@�p�@�G�@�%@��@��j@���@�z�@�Z@��F@�\)@�+@�
=@���@�J@��T@�@���@�`B@���@�r�@�I�@���@�|�@��@��R@���@��\@�M�@�V@�V@���@��7@�hs@�G�@�/@�7L@�7L@��@��@�A�@�  @���@�  @�1@�b@� �@��;@�|�@�\)@�S�@�S�@�"�@�
=@�^5@��@��-@�&�@��@�j@�@~�@}�@|Z@{"�@z-@y�@zJ@zM�@z^5@y�@x��@xb@v��@vE�@v@u�T@u�h@u/@u��@wK�@w�@w�w@w|�@u�h@t��@t(�@s��@sƨ@v�R@w+@w|�@wl�@w;d@w�@v�+@uO�@uV@t�j@t�@tZ@s��@s"�@r�H@r�H@s"�@so@r��@r-@q�@p��@p�u@pb@ol�@n��@n�R@nff@n5?@n@m�T@m@mO�@l�@l��@lI�@l(�@l1@k�m@kdZ@j��@jJ@i�7@hr�@g�@g;d@f��@f5?@f��@fv�@fV@f{@e�h@d��@dZ@d9X@d1@cƨ@c��@ct�@c33@b��@bJ@a�^@a��@ax�@aG�@`�`@_�@_�P@_;d@^�@^�+@^ff@^@]�@]?}@\��@\Z@\�@\1@[�m@[�@Z�!@Z~�@ZM�@Z=q@Z�@Y�@Y��@YX@YG�@Y&�@XĜ@XA�@Xb@W�w@W�P@WK�@V�y@U�@U?}@T��@Tz�@T�@Sƨ@SC�@R��@R�\@Q��@Q��@Q�7@P�9@PQ�@PA�@P �@Pb@P  @O�@O�;@O��@O��@O
=@N�y@N�@N�R@N��@Nff@N{@M�-@M/@L�@Lz�@LZ@L(�@K�F@KS�@KC�@KC�@J�H@J~�@JM�@J-@J-@J-@J-@I�@I�7@H�`@HQ�@Hb@G�;@G��@G|�@G\)@G;d@G;d@G;d@G;d@G+@G�@Fȴ@F��@F��@F5?@E��@Dj@D(�@C��@C�
@C�
@C�F@C��@C�@C�@Ct�@CS�@CC�@Co@B�\@B�@B-@BJ@BJ@A�@Ax�@Ahs@AX@@�`@@��@@�@@r�@@Q�@@b@?;d@?
=@>�@>�R@>��@>V@>$�@=��@=�-@=�-@=@=@=�h@=`B@<�@<��@<�D@<j@<9X@;��@;�
@;��@;33@:�!@:-@9��@9�#@9��@9%@8��@8�9@8�u@8�@8bN@8A�@8 �@7�@7�@7;d@6ȴ@6��@6v�@6V@6E�@6{@5��@5�h@5`B@5O�@5/@5�@5V@4��@3��@2�@2��@2�!@2��@2M�@1�#@1�@0��@0r�@/�@/l�@/l�@/\)@.��@.v�@.E�@.E�@.$�@.@-�T@-��@-p�@-�@,�/@,��@,(�@+��@+�m@+��@+�@+C�@*=q@*J@)��@)��@)��@)�7@)x�@)x�@)hs@)G�@)&�@)�@)�@)%@(�`@(�9@(b@'�w@'��@'|�@'\)@'�@'
=@&�@&V@%�T@%�@%/@%�@%V@$��@$�@$�/@$��@$�j@$�D@$Z@$1@#��@#C�@"�H@"n�@"J@!��@!�7@!hs@!X@!7L@!%@ Ĝ@ �u@ Q�@   @�P@K�@;d@
=@�@�R@��@��@v�@ff@ff@{@@�-@��@`B@V@��@�@��@z�@9X@��@dZ@@n�@-@�@J@��@�@��@��@x�@hs@X@&�@��@r�@��@+@�y@ȴ@��@ff@$�@@��@O�@�j@9X@1@��@dZ@33@@�H@�!@��@~�@M�@=q@�@�@��@�^@��@X@7L@&�@�@%@�9@�@r�@bN@Q�@1'@  @�@K�@�@ȴ@�R@��@v�@V@E�@$�@@�h@`B@?}@�@�@�@z�@Z@I�@(�@�@ƨ@t�@dZ@33@
�@
��@
�\@
M�@
�@
J@
J@
J@	�@	�#@	��@	��@	��@	�^@	hs@	X@	&�@	%@�`@�9@��@�@bN@A�@A�@A�@1'@ �@1'@1'@ �@�@�w@�P@
=@��@E�@$�@�T@�-@�@p�@p�@`B@?}@/@V@�@��@�j@�j@�@z�@j@9X@1@��@��@��@�m@�
@ƨ@ƨ@ƨ@��@�@�@t�@t�@t�@S�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�E�A�?}A�5?A�1'A�1'A�1'A�1'A�1'A�1'A�1'A�/A�/A�1'A�1'A�33A�33A�33A�33A�5?A�5?A�5?A��#A�"�A�dZA�(�A�A�A��HA��A��
A��A�VA��A��TA�~�A���A��jA�S�A��yA��A�ffA�G�A��HA�z�A�ĜA�;dA���A��A��A�;dA�p�A�9XA�?}A���A�n�A�`BA��`A�&�A��7A�(�A�ƨA�dZA�bA��PA�bNA��RA�hsA�5?A�%A���A�VA���A�dZA�ZA�?}A���A��!A���A�$�A�JA��A�A��\A��A�=qA~I�A}�7A|�A|{Az�HAy��Aw33Avr�AvbNAvbAuK�At�DAs�
ArffAo�TAm�hAk�FAj��Ai�FAi/Ah�uAgt�AgG�Ag?}Ag"�Af�Ae|�AcƨAa�PA_��A]&�A[dZAZ�!AZ~�AZJAY%AW��AVE�AT�HAS�AQ�wAO�AO7LAM�^AM?}AMAL�HALȴALbNAL  AK/AKVAJ�/AJI�AI\)AH��AH�AHI�AG�-AG�AG7LAF��AE�TAE;dAD�AB��ABjABQ�AB=qAA�TA@�/A@E�A?33A>r�A> �A=��A;��A9��A9l�A8�A8bA7A7�7A7S�A6��A6bNA5��A4�A3
=A1"�A/��A/`BA/�A.��A,�`A+�hA*��A*ffA*9XA)�hA(��A(�A'�PA&^5A%�A"�A!�A!;dA �A�A&�A�`AȴA�9A��A�+AE�A�wAn�AXA��A��Ar�AƨA/AQ�AƨAS�A��A��A�-AZA9XA�-AS�A��A�
A�jAn�A
ĜA(�A\)A��AQ�A�AȴA^5A�#A��A�A ĜA A�@��R@�
=@��@��@��y@���@�&�@��@��y@�?}@��-@땁@��@�-@�V@�ƨ@�t�@�"�@��@�!@噚@��@�A�@�S�@��y@�V@ܴ9@�1@��y@٩�@أ�@�b@�-@�(�@�l�@Ұ!@�=q@���@ёh@�V@�Ĝ@�r�@�Q�@��;@��@�`B@�;d@ʧ�@�M�@���@�&�@�@��@���@��@��#@�O�@���@��@���@��!@�K�@���@��P@���@��/@��u@��u@�z�@�(�@��@��F@�l�@��y@��+@���@�/@�Ĝ@�r�@�(�@�=q@�V@�Ĝ@��@�A�@���@�@���@�V@�p�@��@��u@��@�j@��m@�\)@���@�M�@�ƨ@��@���@�E�@�G�@��@�j@���@�ȴ@�M�@���@��7@�7L@�z�@��@�ƨ@��@��P@�K�@�o@��R@�@���@�X@�33@�V@�E�@�$�@��h@�`B@��7@��@�r�@�  @�dZ@��\@�$�@�J@��@���@�?}@��/@�1'@���@��@�S�@�+@�@��!@���@�/@��9@�A�@�9X@�9X@�  @��
@���@�K�@��@��y@��\@�{@���@��j@��@��\@�v�@��@���@�p�@�G�@�%@��@��j@���@�z�@�Z@��F@�\)@�+@�
=@���@�J@��T@�@���@�`B@���@�r�@�I�@���@�|�@��@��R@���@��\@�M�@�V@�V@���@��7@�hs@�G�@�/@�7L@�7L@��@��@�A�@�  @���@�  @�1@�b@� �@��;@�|�@�\)@�S�@�S�@�"�@�
=@�^5@��@��-@�&�@��@�j@�@~�@}�@|Z@{"�@z-@y�@zJ@zM�@z^5@y�@x��@xb@v��@vE�@v@u�T@u�h@u/@u��@wK�@w�@w�w@w|�@u�h@t��@t(�@s��@sƨ@v�R@w+@w|�@wl�@w;d@w�@v�+@uO�@uV@t�j@t�@tZ@s��@s"�@r�H@r�H@s"�@so@r��@r-@q�@p��@p�u@pb@ol�@n��@n�R@nff@n5?@n@m�T@m@mO�@l�@l��@lI�@l(�@l1@k�m@kdZ@j��@jJ@i�7@hr�@g�@g;d@f��@f5?@f��@fv�@fV@f{@e�h@d��@dZ@d9X@d1@cƨ@c��@ct�@c33@b��@bJ@a�^@a��@ax�@aG�@`�`@_�@_�P@_;d@^�@^�+@^ff@^@]�@]?}@\��@\Z@\�@\1@[�m@[�@Z�!@Z~�@ZM�@Z=q@Z�@Y�@Y��@YX@YG�@Y&�@XĜ@XA�@Xb@W�w@W�P@WK�@V�y@U�@U?}@T��@Tz�@T�@Sƨ@SC�@R��@R�\@Q��@Q��@Q�7@P�9@PQ�@PA�@P �@Pb@P  @O�@O�;@O��@O��@O
=@N�y@N�@N�R@N��@Nff@N{@M�-@M/@L�@Lz�@LZ@L(�@K�F@KS�@KC�@KC�@J�H@J~�@JM�@J-@J-@J-@J-@I�@I�7@H�`@HQ�@Hb@G�;@G��@G|�@G\)@G;d@G;d@G;d@G;d@G+@G�@Fȴ@F��@F��@F5?@E��@Dj@D(�@C��@C�
@C�
@C�F@C��@C�@C�@Ct�@CS�@CC�@Co@B�\@B�@B-@BJ@BJ@A�@Ax�@Ahs@AX@@�`@@��@@�@@r�@@Q�@@b@?;d@?
=@>�@>�R@>��@>V@>$�@=��@=�-@=�-@=@=@=�h@=`B@<�@<��@<�D@<j@<9X@;��@;�
@;��@;33@:�!@:-@9��@9�#@9��@9%@8��@8�9@8�u@8�@8bN@8A�@8 �@7�@7�@7;d@6ȴ@6��@6v�@6V@6E�@6{@5��@5�h@5`B@5O�@5/@5�@5V@4��@3��@2�@2��@2�!@2��@2M�@1�#@1�@0��@0r�@/�@/l�@/l�@/\)@.��@.v�@.E�@.E�@.$�@.@-�T@-��@-p�@-�@,�/@,��@,(�@+��@+�m@+��@+�@+C�@*=q@*J@)��@)��@)��@)�7@)x�@)x�@)hs@)G�@)&�@)�@)�@)%@(�`@(�9@(b@'�w@'��@'|�@'\)@'�@'
=@&�@&V@%�T@%�@%/@%�@%V@$��@$�@$�/@$��@$�j@$�D@$Z@$1@#��@#C�@"�H@"n�@"J@!��@!�7@!hs@!X@!7L@!%@ Ĝ@ �u@ Q�@   @�P@K�@;d@
=@�@�R@��@��@v�@ff@ff@{@@�-@��@`B@V@��@�@��@z�@9X@��@dZ@@n�@-@�@J@��@�@��@��@x�@hs@X@&�@��@r�@��@+@�y@ȴ@��@ff@$�@@��@O�@�j@9X@1@��@dZ@33@@�H@�!@��@~�@M�@=q@�@�@��@�^@��@X@7L@&�@�@%@�9@�@r�@bN@Q�@1'@  @�@K�@�@ȴ@�R@��@v�@V@E�@$�@@�h@`B@?}@�@�@�@z�@Z@I�@(�@�@ƨ@t�@dZ@33@
�@
��@
�\@
M�@
�@
J@
J@
J@	�@	�#@	��@	��@	��@	�^@	hs@	X@	&�@	%@�`@�9@��@�@bN@A�@A�@A�@1'@ �@1'@1'@ �@�@�w@�P@
=@��@E�@$�@�T@�-@�@p�@p�@`B@?}@/@V@�@��@�j@�j@�@z�@j@9X@1@��@��@��@�m@�
@ƨ@ƨ@ƨ@��@�@�@t�@t�@t�@S�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B<jB<jB<jB<jB<jB<jB<jB<jB<jB<jB<jB<jB=qB<jB=qB=qB=qB=qB=qB=qB=qB9XBO�BA�B+B'�B!�B�B+B�B�B�BB��B�}B�RB�?B�!B��B��B�Be`BaHB\)BVBM�BG�B:^B.B�B{BB��B�B�BǮB�}B�RB��B�\B~�Bu�Br�Bk�B_;BT�BO�BL�BI�BE�B;dB7LB1'B1'B/B+B �BPBB
��B
��B
��B
�yB
�B
��B
�hB
�7B
�B
z�B
m�B
dZB
N�B
D�B
C�B
B�B
;dB
6FB
/B
"�B
bB	��B	�B	�yB	�ZB	�BB	�)B	�B	��B	��B	��B	��B	ɺB	��B	�!B	��B	��B	�VB	�1B	�+B	�B	~�B	w�B	q�B	k�B	cTB	]/B	Q�B	N�B	H�B	E�B	D�B	C�B	C�B	A�B	@�B	<jB	;dB	:^B	9XB	5?B	2-B	1'B	0!B	-B	,B	+B	(�B	$�B	#�B	 �B	�B	�B	�B	�B	{B	hB	VB		7B	1B	%B	B��B�B�B�B�sB�mB�fB�`B�TB�HB�;B�)B�B��B��B��B��BɺBB�dB�LB�?B�9B�-B�!B�B�B��B��B��B��B�{B�hB�VB�VB�PB�PB�PB�PB�JB�JB�DB�7B�%B�B�B�B� B}�Bz�Bx�Bu�Bs�Bn�BjBcTBaHB\)B[#BYBVBQ�BO�BN�BC�BA�B>wB=qB=qB:^B9XB8RB5?B49B1'B.B.B)�B&�B&�B$�B#�B"�B!�B�B�B�B�B�B�B�B�B{B{BuBuBhB\BJB
=B	7B1B1B%B%B%BBB+B%B%B%B%B%B%B%B%B%B%B%B+B1BDBDBDBVBDB
=BDBPBPBVB\BoB�B�B&�B+B0!B8RB<jB>wB?}B>wB?}B@�B@�BA�BA�BC�BC�BF�BF�BG�BG�BH�BN�BQ�BR�BR�BS�BT�BXBXBYB\)B]/B^5B^5B^5B`BBaHBcTBdZBn�Bp�Bq�Bs�Bx�B{�B|�B� B�B�B�1B�7B�JB�bB�uB�{B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�'B�?B�RB�^B�jB�wB��B��BBƨBɺB��B��B��B��B��B��B��B�B�
B�
B�B�B�B�
B�
B�B�B�B�B�#B�#B�)B�)B�)B�)B�)B�)B�/B�5B�;B�BB�HB�ZB�mB�sB�B�B�B�B��B��B��B��B��B��B��B	B	B	+B	+B		7B	PB	hB	bB	bB	uB	�B	�B	�B	#�B	%�B	&�B	'�B	(�B	(�B	+B	.B	33B	6FB	7LB	8RB	8RB	8RB	9XB	9XB	=qB	A�B	B�B	B�B	B�B	E�B	E�B	H�B	I�B	J�B	M�B	M�B	K�B	K�B	J�B	J�B	H�B	K�B	P�B	R�B	T�B	XB	YB	ZB	ZB	\)B	]/B	^5B	_;B	_;B	`BB	aHB	e`B	k�B	p�B	r�B	y�B	z�B	z�B	z�B	|�B	�B	�JB	�\B	�bB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�-B	�3B	�9B	�FB	�LB	�RB	�XB	�XB	�^B	�^B	�dB	�jB	�wB	�}B	��B	��B	��B	��B	��B	ÖB	ÖB	ĜB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�)B	�;B	�;B	�BB	�HB	�NB	�TB	�ZB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
1B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B

=B
DB
DB
DB
JB
JB
JB
PB
PB
\B
\B
bB
bB
bB
hB
hB
hB
hB
hB
oB
oB
uB
uB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
$�B
$�B
$�B
$�B
$�B
%�B
%�B
&�B
&�B
&�B
'�B
'�B
'�B
(�B
)�B
)�B
)�B
)�B
+B
+B
+B
+B
,B
-B
.B
.B
.B
/B
0!B
0!B
0!B
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
2-B
33B
33B
33B
49B
49B
49B
49B
49B
49B
49B
5?B
5?B
5?B
6FB
6FB
7LB
7LB
7LB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
;dB
;dB
=qB
=qB
=qB
>wB
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
C�B
C�B
C�B
C�B
D�B
D�B
E�B
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
G�B
G�B
G�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
K�B
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
M�B
N�B
N�B
N�B
N�B
N�B
O�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
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
T�B
T�B
T�B
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
XB
XB
XB
YB
YB
YB
YB
YB
YB
YB
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
]/B
]/B
]/B
]/B
]/B
^5B
^5B
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
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
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
iyB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
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
m�B
n�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B<�B<�B<jB<jB<jB<jB<jB<jB<�B<jB<jB<jB=qB<�B=qB=qB=�B=�B>B?HBC�BK�B_�BJ�B.B,B'�B�B�B�B�]B�FB҉B��B�$B��B��B��B��B��Bf�BbhB]�BWsBO�BJ=B<�B0�B!BYB�B��B��B�QB�RB��B��B�B� B�Bv�BtnBm�B`�BU�BPbBM�BJ�BF�B<PB7�B1vB1�B0�B-�B"�BpB�B
��B
�(B
��B
�B
ݘB
�KB
�oB
�#B
�MB
|jB
o�B
f�B
O�B
D�B
DMB
C{B
<�B
7�B
1AB
%�B
B
;B	�B	�B	�,B	�HB	�/B	�SB	�FB	�uB	�@B	�HB	�0B	�aB	��B	��B	�yB	�BB	��B	��B	��B	��B	y�B	s�B	mwB	e�B	_;B	S&B	PbB	IRB	E�B	D�B	C�B	DMB	BAB	AoB	<�B	;�B	;JB	:xB	5�B	2�B	1�B	0�B	-wB	,�B	+�B	)�B	%�B	%,B	"hB	#B	�B	�B	9B	�B	oB	�B	
#B	�B	+B	_B	 4B�B�B�]B�B��B��B�B�&B�hB�BޞB�xBևBЗBΊB�B��B�3B��B��B��B�%B�B�B��B��B��B�LB�B��B��B�:B��B��B��B��B��B��B��B�jB��B��B��B�{B��B��B�B.B{�By�BwLBu�BpoBl�Be�BbB\�B\)BZ�BWsBSBR�BQ�BD�BBuB?HB>�B>�B;JB:^B9�B6�B5�B2GB/�B0;B*�B'�B($B%�B$�B#�B"�B!HB �B�BB+BEBYB�B�B�BB�BuBbB�B
�B	�B	7B�B�BB�B�B�BKB�B�BtBtBYBtBtBYBYB�B�BfB	�B�BBBB�BxB0BBB�B�B�BB5B&�B*�B0�B9�B<�B>�B?�B>�B?�B@�B@�BA�BB'BDBD3BGBGBH1BHKBJ#BO�BR:BS[BS@BT{BU�BXyBXyBY�B\�B]~B^�B^�B^�B`�Ba�BdBe�BoBp�BrGBt�ByXB|PB}�B��B��B��B��B��B��B��B��B��B��B��B��B�B�1B�B�IB�'B�zB�8B�6B�wB�CB�/B��B��B��B��B�B��B��B��B��B��B�#B�JB�B�NB�:B�,B�,B�gB֡BרB�sB�mB�B�B�YB�?B�_B�QB�kB�kB�qBۦBܬB��B�B��B�]BܒBݘBބBߊB��B�|B�B�B�B��B�B�B��B��B�%B�8B�B�"B�(B�HB	oB	mB	zB	zB		�B	�B	�B	}B	�B	�B	�B	�B	)B	$B	&B	'B	(
B	(�B	)B	+6B	.�B	3�B	6zB	7�B	8lB	8RB	8lB	9�B	9�B	=�B	A�B	B�B	B�B	B�B	E�B	F%B	IB	J#B	K)B	N"B	N<B	LJB	LJB	K)B	K^B	I7B	L0B	Q B	SB	UB	X+B	YeB	Z�B	Z�B	\�B	]~B	^jB	_VB	_VB	`\B	a-B	eB	k�B	p�B	r�B	zxB	{0B	{0B	{B	|�B	�UB	�0B	�\B	�}B	��B	��B	��B	�B	��B	��B	��B	�B	�-B	�B	��B	��B	��B	�*B	�6B	�=B	�IB	�iB	�|B	��B	�nB	��B	��B	��B	�rB	��B	�xB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	��B	��B	��B	�B	�B	�4B	�:B	�2B	�9B	�?B	�+B	�EB	�EB	�_B	�KB	�KB	�7B	�QB	�=B	�=B	�WB	�xB	�pB	ߊB	��B	�|B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	� B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�B	��B	�B	�B	�<B	�BB	�HB
 B
UB
UB
[B
AB
GB
GB
GB
MB
SB
�B
fB
fB
KB
	RB
	RB
	RB
	lB
	RB
	lB

rB
^B
xB
xB
~B
dB
�B
�B
�B
�B
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
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
B
�B
 �B
 �B
 �B
 �B
!B
!�B
!�B
!�B
!�B
#B
#B
"�B
#B
# B
$�B
%B
%B
$�B
$�B
&B
&B
'B
'B
'B
'�B
($B
($B
)*B
*B
*0B
*B
*B
+6B
+B
+6B
+6B
,=B
-]B
./B
./B
./B
/OB
0;B
0;B
0;B
0;B
0UB
0;B
0;B
1AB
1AB
1vB
2GB
2GB
2GB
2aB
2aB
3MB
3hB
3hB
4TB
4TB
4TB
4nB
4TB
4�B
4�B
5tB
5ZB
5ZB
6`B
6�B
7�B
7�B
7�B
8lB
8�B
9�B
9XB
9rB
9�B
9�B
9rB
9XB
9rB
:xB
:xB
:�B
;B
;�B
=�B
=�B
=�B
>�B
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
C�B
C�B
C�B
C�B
D�B
D�B
E�B
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
G�B
G�B
G�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
K�B
L�B
MB
L�B
L�B
MB
MB
M�B
M�B
M�B
NB
M�B
M�B
OB
N�B
N�B
N�B
OB
O�B
N�B
O�B
PB
O�B
PB
PB
PB
PB
QB
QB
Q�B
RB
RB
SB
T,B
TB
TB
T�B
U2B
VB
VSB
V9B
VSB
UMB
U2B
U2B
V9B
V9B
V9B
VB
V9B
VSB
W?B
W?B
XEB
XEB
XEB
Y1B
Y1B
Y1B
Y1B
YB
Y1B
Y1B
ZB
ZQB
Z7B
Z7B
Z7B
[WB
[WB
[WB
[WB
\)B
\]B
\CB
\]B
]IB
]/B
]IB
]IB
]dB
^jB
^jB
_pB
_VB
_;B
_pB
`\B
`\B
`\B
`\B
`\B
`�B
abB
abB
a|B
abB
b�B
b�B
b�B
bNB
b�B
b�B
bhB
b�B
bhB
b�B
c�B
cnB
c�B
cnB
c�B
cTB
cnB
cnB
c�B
cnB
cnB
dZB
dtB
d�B
dtB
e�B
ezB
ezB
e�B
ezB
f�B
f�B
f�B
f�B
gmB
gmB
gmB
g�B
gmB
g�B
g�B
g�B
h�B
h�B
h�B
i�B
j�B
j�B
j�B
k�B
k�B
k�B
k�B
k�B
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
m�B
n�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<��<��I<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.08(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201609100036082016091000360820160910003608201806221213332018062212133320180622121333201804050406082018040504060820180405040608  JA  ARFMdecpA19c                                                                20160907033508  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20160906183735  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20160906183735  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20160906183735  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20160906183736  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20160906183736  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20160906183736  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20160906183736  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20160906183736  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20160906183737                      G�O�G�O�G�O�                JA  ARUP                                                                        20160906192502                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20160907154417  CV  JULD            G�O�G�O�F�E�                JM  ARCAJMQC2.0                                                                 20160909153608  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20160909153608  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404190608  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622031333  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115111518                      G�O�G�O�G�O�                