CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-06-24T06:44:29Z creation;2016-06-24T06:44:31Z conversion to V3.1;2019-12-19T08:35:28Z update;     
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
_FillValue                 �  IT   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  MD   PRES_ADJUSTED_QC         
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
_FillValue                 �  �L   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �<   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
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
resolution        :�o     �  �D   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ޴   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
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
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  20160624064429  20200116201516  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  I2_0577_003                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @ױ���1   @ױ	b� @3��!�.I�dǄM:�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�fC�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�fD�@ D��fD�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��
@�p�@�p�A�RA>�RA^�RA~�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B��
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
C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D	z�D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D#��D$z�D$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtz�Dt��Duz�Du��Dvz�Dv��Dwz�Dw��Dxz�Dx��Dyz�Dy��Dzz�Dz��D{z�D{��D|z�D|��D}z�D}��D~z�D~��Dz�D��D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD���D��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD� �D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD½qD��qD�=qD�}qDýqD��qD�=qD�}qDĽqD��qD�=qD�}qDŽqD��qD�=qD�}qDƽqD��qD�=qD�}qDǽqD��qD�=qD�}qDȽqD��qD�=qD�}qDɽqD��qD�=qD�}qDʽqD��qD�=qD�}qD˽qD��qD�=qD�}qD̽qD��qD�=qD�}qDͽqD��qD�=qD�}qDνqD��qD�=qD�}qDϽqD��qD�=qD�}qDнqD��qD�=qD�}qDѽqD��qD�=qD�}qDҽqD��qD�=qD�}qDӽqD��qD�=qD�}qDԽqD��qD�=qD�}qDսqD��qD�=qD�}qDֽqD��qD�=qD�}qD׽qD��qD�=qD�}qDؽqD��qD�=qD�}qDٽqD��qD�=qD�}qDڽqD��qD�=qD�}qD۽qD��qD�=qD�}qDܽqD��qD�=qD�}qDݽqD��qD�=qD�}qD޽qD��qD�=qD�}qD߽qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��D�=qD���D��q1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�l�A�jA�hsA�dZA�`BA�`BA�?}A���A��HA���Aִ9A֥�A։7A���A�%Aԡ�A�r�A�M�A�+A��`AӺ^AӶFA�ĜAӛ�A�ZAҏ\A�(�Aћ�A�=qAиRA�ZA��A�bNA΅A� �A���A�+A�ZA�x�AʬA�7LAɥ�A�33AȰ!A��A�O�A�bNA���A�A���A���A���A�A�A�%A�hsA�/A���A�A���A��
A�ĜA���A�hsA�{A��A�
=A�/A�ƨA�
=A��A��7A�?}A���A�%A�I�A��A�K�A�ȴA��A��A�A���A�
=A�oA���A�~�A�1'A��`A��\A� �A��+A�=qA~�+A|�HA|�AyC�Av�+At��As�
Aq�An�Ak�Ajv�Aix�Ag�
Af$�Ad��Ad5?Ac�Ab��A`��A^VA[�7AY�AW��AU�7AS��AQ��APffAP�AN��ANĜAN��AN�AN�uAMt�AK�FAJ(�AHȴAG�FAF-AD�AC|�AB��AA��AA7LA=�hA;�A;oA9�
A6��A41A2��A0ĜA/A/;dA/%A.�!A-�TA,r�A+�TA+`BA*��A)�PA(�A(  A&�A%�A$�/A$ZA#
=A!�hA ��A AffAJA�TA�A��A1'At�AC�A�+A�FA|�A��AM�A�-A�AdZAn�AQ�A�A
ȴA�A^5A�uA	&�A	��A
n�AhsAVAE�A��Ar�A��A�A-A��A�A��AdZA Z@�%@��@��9@�;d@�X@���@�`B@�@�I�@���@�r�@��y@���@��H@�@�V@��@�
=@�1'@�=q@��u@�$�@ܣ�@ۅ@�o@��@ڧ�@ڇ+@���@��H@�?}@�(�@�+@�$�@�J@�$�@љ�@�G�@с@��`@ЋD@�r�@�bN@�l�@�@�X@��@��@˝�@���@�V@ȋD@Ǯ@��@�v�@ř�@�p�@�7L@�hs@š�@�p�@�"�@�X@�@�/@�1'@�  @��;@�C�@���@�5?@�$�@��@��@��T@��m@�S�@���@�/@���@�r�@�b@��@��@���@�`B@��@��`@��@�x�@�+@���@�V@���@�A�@��@��P@���@���@�p�@�G�@�Ĝ@�A�@� �@���@�l�@���@��+@�ff@���@�=q@�~�@�E�@��-@�x�@�X@�V@�&�@���@���@�(�@� �@�b@�  @���@��;@��@�K�@�ȴ@�E�@�5?@�$�@��R@���@��@��@�`B@�7L@�/@��@��@���@��@�z�@�r�@�9X@���@�@�n�@�v�@�M�@���@�bN@�;d@��@�ƨ@�1'@���@���@��!@���@��y@�"�@�33@�|�@���@�n�@���@��@�dZ@��@�K�@�+@���@���@���@���@�9X@��P@�33@��@��y@�V@��h@�O�@�7L@�&�@���@���@��u@�z�@�Z@�A�@�  @���@�K�@�l�@�t�@��@��@���@�Q�@���@�bN@�1'@���@�+@��!@�^5@�J@��7@�G�@�%@���@��@�I�@�(�@��@�1@��m@���@�|�@�S�@�
=@��!@�5?@��h@�x�@�O�@��@���@��9@��@�r�@���@�C�@��@�ff@�ff@��T@�/@��@���@��j@���@�bN@�  @��w@��w@��w@��@�S�@�S�@�S�@�K�@�"�@��@�"�@��@��!@�E�@���@�@�`B@�V@��@���@��@�  @��F@�+@�ȴ@�ff@���@��#@��-@��/@�z�@�I�@��@�o@�~�@�=q@�J@��#@���@���@���@�?}@��/@��j@��@�Z@�Q�@��@��m@�ƨ@��w@�;d@���@�-@��#@�@��^@��h@�X@�%@��j@��@�r�@�Q�@� �@��@�b@��w@�K�@�@�ȴ@��!@���@��+@���@��\@�{@���@��T@��7@�`B@�G�@�V@��j@���@��D@�bN@�A�@�A�@�1'@�1'@��@�@�P@�@~�R@~V@~@}��@}�@|�/@|�D@|9X@|(�@|9X@|�@|1@{�m@{�F@{��@{dZ@{33@{@z�!@z~�@zM�@zM�@zM�@z�@y��@y�7@y7L@y7L@x1'@v�y@v$�@u��@u�@uV@t�j@tI�@s��@s33@r�H@r~�@r�@q��@q��@qhs@q7L@q�@pA�@o�@o�@o�w@o�P@ol�@oK�@o
=@n��@m�@m?}@mV@lZ@l9X@l�@l1@kt�@j�!@j^5@ihs@h�9@g�@g�@fȴ@f$�@e�@e��@e��@e�h@ep�@d�/@d�@dj@d(�@c�
@c�
@c�F@cS�@cC�@cC�@co@b�\@b�@a�7@a7L@`��@`bN@`A�@`1'@_�@_\)@^ȴ@^ff@^E�@^{@]@]�@]�@\��@\�j@\�j@\�@[��@[��@[�@[�@[S�@Z��@Y��@YX@X��@X�9@XQ�@W�w@W|�@W�@Vȴ@V��@Vv�@V5?@V@U@UO�@U�@U�@T��@T��@T1@SS�@S@R�H@R��@R�\@Rn�@Qhs@Pr�@PQ�@P1'@Pb@O��@O�P@O|�@O|�@O|�@O+@N�@N��@L�@L9X@K�@Ko@Ko@K@J�@I�#@I��@H�`@H�9@Hr�@HQ�@H1'@Hb@G�;@Gl�@F��@F$�@E��@D�@Dj@DI�@C�m@Ct�@C"�@Co@B�H@B�!@B=q@A�#@A��@A��@@��@@bN@@A�@?�P@?K�@?;d@?�@>ȴ@>$�@=@=��@=�@=`B@=V@<j@;"�@:�!@:M�@9��@9hs@9&�@8��@8Ĝ@8�@81'@7�@7K�@6ȴ@6V@5�@5@5�@5/@4��@4z�@4Z@4(�@3��@3�@3S�@3C�@3o@2�@2��@2=q@1x�@1G�@1&�@0��@0Ĝ@0Ĝ@0Ĝ@0Ĝ@0r�@/l�@.��@.�R@.��@.v�@.5?@-��@-?}@,��@,j@,1@+��@+@*~�@*^5@*-@)��@)G�@(��@(bN@(Q�@(1'@'�w@'|�@';d@&�y@&5?@%�@&@&@%��@%O�@%�@$��@$�/@$�D@$j@$j@$j@$j@$(�@#��@#�
@#t�@"�!@"n�@"n�@"n�@"^5@"M�@!�@!�^@!��@!hs@!&�@ �`@ ��@ Ĝ@ Ĝ@ �9@ ��@ �@ �@ �@ �@ �@ Q�@ b@�@�;@�@\)@;d@�@
=@��@�R@��@ff@{@@�@@�h@`B@O�@?}@?}@?}@O�@?}@?}@/@�@z�@�m@�@dZ@33@"�@�H@��@�!@��@�\@~�@J@�#@��@��@�^@�7@hs@G�@7L@&�@&�@�@%@�`@�u@Q�@ �@b@b@�@�;@�P@l�@+@
=@�y@ȴ@ȴ@�@�@�@�@ȴ@ȴ@ȴ@�+@5?@5?@��@��@`B@`B@O�@�@V@��@��@��@�/@j@(�@�@�
@�F@��@t�@dZ@C�@o@�@��@��@��@��@��@��@��@�!@n�@^5@M�@-@��@�@��@%@Ĝ@�9@�u@1'@�w@�@��@��@l�@�@ȴ@ȴ@�R@V@@�@�T@@p�@�@�/@�@�D@�D@(�@��@dZ@C�@o@@
�@
��@
~�@
^5@
M�@
J@	��@	��@	�71111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�l�A�jA�hsA�dZA�`BA�`BA�?}A���A��HA���Aִ9A֥�A։7A���A�%Aԡ�A�r�A�M�A�+A��`AӺ^AӶFA�ĜAӛ�A�ZAҏ\A�(�Aћ�A�=qAиRA�ZA��A�bNA΅A� �A���A�+A�ZA�x�AʬA�7LAɥ�A�33AȰ!A��A�O�A�bNA���A�A���A���A���A�A�A�%A�hsA�/A���A�A���A��
A�ĜA���A�hsA�{A��A�
=A�/A�ƨA�
=A��A��7A�?}A���A�%A�I�A��A�K�A�ȴA��A��A�A���A�
=A�oA���A�~�A�1'A��`A��\A� �A��+A�=qA~�+A|�HA|�AyC�Av�+At��As�
Aq�An�Ak�Ajv�Aix�Ag�
Af$�Ad��Ad5?Ac�Ab��A`��A^VA[�7AY�AW��AU�7AS��AQ��APffAP�AN��ANĜAN��AN�AN�uAMt�AK�FAJ(�AHȴAG�FAF-AD�AC|�AB��AA��AA7LA=�hA;�A;oA9�
A6��A41A2��A0ĜA/A/;dA/%A.�!A-�TA,r�A+�TA+`BA*��A)�PA(�A(  A&�A%�A$�/A$ZA#
=A!�hA ��A AffAJA�TA�A��A1'At�AC�A�+A�FA|�A��AM�A�-A�AdZAn�AQ�A�A
ȴA�A^5A�uA	&�A	��A
n�AhsAVAE�A��Ar�A��A�A-A��A�A��AdZA Z@�%@��@��9@�;d@�X@���@�`B@�@�I�@���@�r�@��y@���@��H@�@�V@��@�
=@�1'@�=q@��u@�$�@ܣ�@ۅ@�o@��@ڧ�@ڇ+@���@��H@�?}@�(�@�+@�$�@�J@�$�@љ�@�G�@с@��`@ЋD@�r�@�bN@�l�@�@�X@��@��@˝�@���@�V@ȋD@Ǯ@��@�v�@ř�@�p�@�7L@�hs@š�@�p�@�"�@�X@�@�/@�1'@�  @��;@�C�@���@�5?@�$�@��@��@��T@��m@�S�@���@�/@���@�r�@�b@��@��@���@�`B@��@��`@��@�x�@�+@���@�V@���@�A�@��@��P@���@���@�p�@�G�@�Ĝ@�A�@� �@���@�l�@���@��+@�ff@���@�=q@�~�@�E�@��-@�x�@�X@�V@�&�@���@���@�(�@� �@�b@�  @���@��;@��@�K�@�ȴ@�E�@�5?@�$�@��R@���@��@��@�`B@�7L@�/@��@��@���@��@�z�@�r�@�9X@���@�@�n�@�v�@�M�@���@�bN@�;d@��@�ƨ@�1'@���@���@��!@���@��y@�"�@�33@�|�@���@�n�@���@��@�dZ@��@�K�@�+@���@���@���@���@�9X@��P@�33@��@��y@�V@��h@�O�@�7L@�&�@���@���@��u@�z�@�Z@�A�@�  @���@�K�@�l�@�t�@��@��@���@�Q�@���@�bN@�1'@���@�+@��!@�^5@�J@��7@�G�@�%@���@��@�I�@�(�@��@�1@��m@���@�|�@�S�@�
=@��!@�5?@��h@�x�@�O�@��@���@��9@��@�r�@���@�C�@��@�ff@�ff@��T@�/@��@���@��j@���@�bN@�  @��w@��w@��w@��@�S�@�S�@�S�@�K�@�"�@��@�"�@��@��!@�E�@���@�@�`B@�V@��@���@��@�  @��F@�+@�ȴ@�ff@���@��#@��-@��/@�z�@�I�@��@�o@�~�@�=q@�J@��#@���@���@���@�?}@��/@��j@��@�Z@�Q�@��@��m@�ƨ@��w@�;d@���@�-@��#@�@��^@��h@�X@�%@��j@��@�r�@�Q�@� �@��@�b@��w@�K�@�@�ȴ@��!@���@��+@���@��\@�{@���@��T@��7@�`B@�G�@�V@��j@���@��D@�bN@�A�@�A�@�1'@�1'@��@�@�P@�@~�R@~V@~@}��@}�@|�/@|�D@|9X@|(�@|9X@|�@|1@{�m@{�F@{��@{dZ@{33@{@z�!@z~�@zM�@zM�@zM�@z�@y��@y�7@y7L@y7L@x1'@v�y@v$�@u��@u�@uV@t�j@tI�@s��@s33@r�H@r~�@r�@q��@q��@qhs@q7L@q�@pA�@o�@o�@o�w@o�P@ol�@oK�@o
=@n��@m�@m?}@mV@lZ@l9X@l�@l1@kt�@j�!@j^5@ihs@h�9@g�@g�@fȴ@f$�@e�@e��@e��@e�h@ep�@d�/@d�@dj@d(�@c�
@c�
@c�F@cS�@cC�@cC�@co@b�\@b�@a�7@a7L@`��@`bN@`A�@`1'@_�@_\)@^ȴ@^ff@^E�@^{@]@]�@]�@\��@\�j@\�j@\�@[��@[��@[�@[�@[S�@Z��@Y��@YX@X��@X�9@XQ�@W�w@W|�@W�@Vȴ@V��@Vv�@V5?@V@U@UO�@U�@U�@T��@T��@T1@SS�@S@R�H@R��@R�\@Rn�@Qhs@Pr�@PQ�@P1'@Pb@O��@O�P@O|�@O|�@O|�@O+@N�@N��@L�@L9X@K�@Ko@Ko@K@J�@I�#@I��@H�`@H�9@Hr�@HQ�@H1'@Hb@G�;@Gl�@F��@F$�@E��@D�@Dj@DI�@C�m@Ct�@C"�@Co@B�H@B�!@B=q@A�#@A��@A��@@��@@bN@@A�@?�P@?K�@?;d@?�@>ȴ@>$�@=@=��@=�@=`B@=V@<j@;"�@:�!@:M�@9��@9hs@9&�@8��@8Ĝ@8�@81'@7�@7K�@6ȴ@6V@5�@5@5�@5/@4��@4z�@4Z@4(�@3��@3�@3S�@3C�@3o@2�@2��@2=q@1x�@1G�@1&�@0��@0Ĝ@0Ĝ@0Ĝ@0Ĝ@0r�@/l�@.��@.�R@.��@.v�@.5?@-��@-?}@,��@,j@,1@+��@+@*~�@*^5@*-@)��@)G�@(��@(bN@(Q�@(1'@'�w@'|�@';d@&�y@&5?@%�@&@&@%��@%O�@%�@$��@$�/@$�D@$j@$j@$j@$j@$(�@#��@#�
@#t�@"�!@"n�@"n�@"n�@"^5@"M�@!�@!�^@!��@!hs@!&�@ �`@ ��@ Ĝ@ Ĝ@ �9@ ��@ �@ �@ �@ �@ �@ Q�@ b@�@�;@�@\)@;d@�@
=@��@�R@��@ff@{@@�@@�h@`B@O�@?}@?}@?}@O�@?}@?}@/@�@z�@�m@�@dZ@33@"�@�H@��@�!@��@�\@~�@J@�#@��@��@�^@�7@hs@G�@7L@&�@&�@�@%@�`@�u@Q�@ �@b@b@�@�;@�P@l�@+@
=@�y@ȴ@ȴ@�@�@�@�@ȴ@ȴ@ȴ@�+@5?@5?@��@��@`B@`B@O�@�@V@��@��@��@�/@j@(�@�@�
@�F@��@t�@dZ@C�@o@�@��@��@��@��@��@��@��@�!@n�@^5@M�@-@��@�@��@%@Ĝ@�9@�u@1'@�w@�@��@��@l�@�@ȴ@ȴ@�R@V@@�@�T@@p�@�@�/@�@�D@�D@(�@��@dZ@C�@o@@
�@
��@
~�@
^5@
M�@
J@	��@	��@	�71111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B�B�B�B�B�B�B�B{B{B�B�B&�B[#B]/BZBXBW
B`BBdZBhsBr�Bz�By�Bw�Bw�Bs�Bo�Bk�BiyBiyBffBu�B{�B�B�=B�{B��B�3B�jB��B�B�BBVB�B,BB�B^5Bk�Bw�B�7B�uB��B��B��B��B��B�uBu�BjBM�B�BJBB�B�fB��B�B�DB�Bo�Bm�Bo�Bm�BgmBdZBaHBVBH�BB�B8RB$�B�BC�BG�B1'BuB
�B
��B
�1B
o�B
O�B
?}B
8RB
'�B
hB
B
  B	�B	�/B	ŢB	�qB	�RB	�!B	��B	��B	��B	�uB	�bB	�B	z�B	n�B	]/B	R�B	H�B	=qB	9XB	7LB	8RB	33B	1'B	0!B	0!B	/B	)�B	�B	{B	
=B	B��B��B�B�B�B�yB�NB�#B�
B��B��BƨBÖB��B�qB�qB�jB�qB�qB�qB�XB�RB�XB�RB�9B�9B�-B�B�B�B�B��B��B��B�-B�-B�9B�RB�wB�jB�^B�XB�XB�LB�?B�B��B�bB�=B�1B�B�B�B~�Bw�By�B}�B�+B�VB��B��B�B��B�hB�PB�=B�B}�Bz�By�By�Bx�Bu�Bm�BiyBn�Bm�Br�B�B�\B��B��B��B��B��B�B�3B�9B�LB�RB�LB�jB�^B�LB�9B�9B�3B�3B�?B�9B�3B�'B�'B�!B�B�B�B�B�3B�3B�FB��BÖBÖBBBĜBƨBɺB��B��B��B��BɺBȴBŢBĜB��B��BȴB��B��B��B��B�B�B�mB�B�B�B�B�B��B��B��B	  B	
=B	VB	oB	oB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	&�B	+B	5?B	?}B	D�B	F�B	J�B	J�B	J�B	K�B	N�B	P�B	R�B	T�B	ZB	`BB	`BB	cTB	ffB	gmB	iyB	m�B	p�B	s�B	t�B	r�B	r�B	s�B	v�B	{�B	|�B	~�B	�B	�B	�B	�B	�B	�B	�B	�%B	�B	�B	�+B	�JB	�hB	�{B	�uB	�uB	�uB	�oB	�oB	�oB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�-B	�?B	�FB	�RB	�RB	�RB	�XB	�^B	�wB	��B	B	ĜB	ĜB	ÖB	B	��B	��B	��B	�}B	�}B	�}B	��B	ÖB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�5B	�HB	�NB	�NB	�TB	�ZB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
1B
1B
	7B

=B

=B
JB
JB
JB
JB
DB

=B
DB
JB
JB
JB
JB
JB
VB
VB
VB
\B
\B
\B
\B
bB
bB
\B
bB
bB
bB
bB
oB
oB
uB
uB
{B
{B
�B
{B
{B
{B
{B
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
 �B
 �B
!�B
!�B
!�B
!�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
,B
,B
,B
,B
,B
-B
-B
-B
-B
-B
.B
/B
0!B
0!B
/B
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
6FB
6FB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
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
:^B
;dB
;dB
<jB
<jB
<jB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
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
B�B
B�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
E�B
E�B
F�B
G�B
G�B
G�B
H�B
G�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
J�B
K�B
K�B
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
O�B
O�B
O�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
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
T�B
T�B
T�B
T�B
T�B
T�B
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
W
B
W
B
XB
XB
XB
YB
YB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
]/B
^5B
`BB
`BB
aHB
bNB
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
dZB
dZB
e`B
ffB
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
hsB
hsB
hsB
iyB
iyB
iyB
iyB
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
m�B
m�B
m�B
m�B
n�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
o�B
o�B
p�B
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
r�B
r�B
r�B
s�B
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
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
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
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B�B�B�BBB�B�B�B�BB
B(�B[�B]~BZkBXyBW�B`�BdZBh�Bs3B{�B{JBx�Bx�Bt�Bp�BlqBjKBkBh
Bv�B|�B��B�B�SB��B�9B��B��B��B�B�BB/BEmB`�BnIB{�B��B�{B��B�dB�]B�WB��B�eBz�BoOBR B"B(B�B�ZB�eBּB�;B��B��Br�BoBq[BoBhsBfLBd�BX+BJ=BE9B;�B&�BxBFtBJ�B5tBB
�$B
��B
��B
s�B
RB
AoB
;�B
*�B
uB
+B
�B	�%B	�B	�EB	��B	�^B	�-B	��B	�qB	�sB	�MB	�[B	�mB	~BB	q[B	_�B	U�B	K)B	?�B	;B	8B	9rB	3�B	1vB	0oB	0�B	0�B	,"B	�B	9B	�B	B	  B�B��B��B�qB�wB�ZB��B�eB��B�B��B��B��B�BB��B�<B��B�.B�BB�DB��B�B��B�ZB��B��B�;B�/B��B��B�KB�XB��B��B��B��B��B��B�VB��B�xB�xB��B�B�iB��B�oB�DB�lB��B�B��B��BxlBy�B}VB��B��B�+B�_B�/B�nB��B�(B�6B�B~wB{0BzDBz�Bz�BxBoiBj�Bo�Bn�BsB��B��B�B�@B�B�$B��B��B��B�B�8B�XB�>B��B��B��B�?B�B��B�hB��B��B�nB��B�aB�B��B��B�WB�OB��B�hB�`B��B��B��B��B�{BňB�EB�#B�0B��B��B�JB�=B�RB�?B�B�B��B��B��B�BуB�uB�
B�1B�
B�/B��B��B�GB�B�B��B�<B	 4B	
�B	�B	B	uB	�B	�B	�B	1B	kB	qB	�B	�B	�B	�B	 �B	&�B	*B	4�B	?cB	D�B	G+B	KB	K)B	KxB	L~B	OB	Q4B	S[B	UgB	ZkB	`�B	`�B	c�B	f�B	g�B	iyB	m�B	p�B	s�B	uB	r�B	r�B	tB	v�B	|B	}VB	cB	�AB	�-B	�GB	�AB	�-B	�GB	��B	��B	��B	�3B	�EB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�7B	��B	��B	��B	�sB	�$B	�SB	��B	��B	��B	�wB	�wB	�cB	�B	� B	�-B	�?B	�`B	��B	��B	�RB	�>B	�DB	�wB	��B	��B	�B	�B	��B	�-B	�'B	��B	��B	��B	��B	�B	�B	��B	��B	��B	�B	�B	�B	�B	��B	�B	�(B	�HB	� B	�B	�B	�B	��B	��B	�	B	�5B	�|B	�B	�B	�B	��B	�B	�B	��B	��B	�B	�B	��B	��B	��B	�B	��B	��B	��B	�B	��B	� B	�B	�B	�B	��B	��B	��B	�B	��B	��B	�%B	�?B	�+B	�B	�+B	��B	�+B	�+B	��B	�B	�$B	�B	�B	�JB	�B	��B	�B
 4B
 B
'B
B
'B
AB
'B
'B
GB
aB
aB
GB
MB
gB
mB
SB
mB
�B
KB
�B
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
~B
�B
pB
pB
pB
�B
�B
vB
vB
�B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
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
!HB
!-B
!B
 �B
!B
!�B
!�B
!�B
"B
#B
#�B
$&B
#�B
$B
$B
$B
$�B
&B
&2B
%�B
%�B
&B
&B
'B
'B
'B
'8B
(>B
($B
(
B
($B
)B
*B
*B
*KB
*eB
*0B
*KB
+kB
,WB
,=B
,WB
,=B
,"B
-CB
-CB
-CB
-)B
-CB
./B
/OB
0;B
0;B
/5B
0;B
0;B
0;B
0;B
0UB
1[B
1vB
1[B
2GB
2GB
2aB
2aB
2GB
3MB
3hB
3�B
3hB
3MB
3hB
4TB
4nB
4�B
5tB
5?B
5ZB
5ZB
5�B
6`B
6FB
6zB
6`B
6zB
6�B
7�B
7�B
7fB
7�B
8�B
8lB
8lB
8lB
9�B
9rB
9rB
9�B
9�B
9�B
9rB
:xB
:xB
:�B
:�B
;�B
;�B
<�B
<�B
<�B
=�B
=�B
>�B
>�B
>�B
>�B
>�B
>�B
>wB
>�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
?�B
?�B
?�B
?�B
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
B�B
B�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
E�B
E�B
F�B
G�B
G�B
G�B
H�B
G�B
H�B
IB
H�B
I�B
I�B
I�B
I�B
J#B
KDB
K�B
LB
MB
MB
M�B
M�B
M�B
M�B
NB
OB
OB
OB
P.B
O�B
O�B
Q B
QB
Q B
QB
R B
RB
RB
R B
S&B
S&B
SB
SB
S&B
S@B
S&B
T,B
T,B
TB
T�B
UB
UB
UB
U2B
UgB
V9B
W$B
W?B
W$B
W$B
W?B
W?B
WYB
XEB
X_B
XEB
YKB
YKB
ZQB
Z7B
ZQB
ZQB
[qB
[=B
[WB
[=B
[WB
\CB
\]B
\xB
]~B
^jB
`\B
`\B
abB
bhB
c�B
c�B
cnB
cnB
dZB
dtB
dtB
dZB
dtB
d�B
d�B
d�B
d�B
e�B
ffB
e`B
ezB
f�B
f�B
f�B
f�B
f�B
f�B
f�B
g�B
gmB
g�B
g�B
gmB
g�B
gmB
g�B
g�B
gmB
g�B
g�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
i�B
i�B
i�B
iyB
i�B
i�B
i�B
j�B
jB
j�B
jB
jB
jB
jB
j�B
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
k�B
k�B
k�B
l�B
m�B
m�B
m�B
m�B
n�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
o�B
o�B
p�B
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
r�B
r�B
r�B
s�B
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
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
xB
xB
x�B
x�B
y	B
x�B
x�B
y�B
y�B
y�B
zB
y�B
zB
z�B
z�B
{B
{0B
{B
{B
|B
|B
|B
|B
|B
}B
}"B
}B
}"B
~B
~B
~(B
}�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.08(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201606061011162016060610111620160606101116201806221257222018062212572220180622125722201804050655342018040506553420180405065534  JA  ARFMdecpA19c                                                                20160624153528  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20160624064429  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20160624064429  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20160624064429  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20160624064430  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20160624064430  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20160624064430  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20160624064430  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20160624064430  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20160624064431                      G�O�G�O�G�O�                JA  ARUP                                                                        20160624071800                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20160606010953  CV  JULD            G�O�G�O�F��F                JM  ARGQJMQC2.0                                                                 20160606010953  CV  JULD_LOCATION   G�O�G�O�F��Z                JM  ARGQJMQC2.0                                                                 20160606010953  CV  LATITUDE        G�O�G�O�A���                JM  ARGQJMQC2.0                                                                 20160606010953  CV  LONGITUDE       G�O�G�O��&<�                JM  ARCAJMQC2.0                                                                 20160606011116  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20160606011116  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404215534  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622035722  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116201516                      G�O�G�O�G�O�                