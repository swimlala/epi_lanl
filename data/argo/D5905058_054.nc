CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-04-18T09:35:34Z creation;2018-04-18T09:35:36Z conversion to V3.1;2019-12-23T06:23:23Z update;     
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \|   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `\   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �|   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  �\   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ܼ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �    HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �`   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �p   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �t   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20180418093534  20200120021523  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               6A   JA  I2_0675_054                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @�\
��.�1   @�\www�@6���[W?�b���n�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�33A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@Q�@���@���Az�A$z�ADz�Adz�A�=qA�=qA�=qA�p�A�=qA�=qA�=qA�=qB�B	�B�B�B!�B)�B1�B9�BA�BI�BQ�BY�Ba�Bi�Bq�By�B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\Bď\Bȏ\B̏\BЏ\Bԏ\B؏\B܏\B��\B�\B�\B�\B��\B�\B��\B��\C G�CG�CG�CG�CG�C
G�CG�CG�CG�CG�CG�CG�CG�CG�CG�CG�C G�C"G�C$G�C&G�C(G�C*G�C,G�C.G�C0G�C2G�C4G�C6G�C8G�C:G�C<G�C>G�C@G�CBG�CDG�CFG�CHG�CJG�CLG�CNG�CPG�CRG�CTG�CVG�CXG�CZG�C\G�C^G�C`G�CbG�CdG�CfG�ChG�CjG�ClG�CnG�CpG�CrG�CtG�CvG�CxG�CzG�C|G�C~G�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�
C�#�C�#�C�0�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D��D�H�D���D���D��D�H�D���D���D��D�H�D���D���D��D�H�D���D���D��D�H�D���D���D��D�H�D���D���D��D�H�D���D���D��D�H�D���D���D��D�H�D���D���D�)D�H�D���D���D��D�H�D���D���D��D�H�D���D���D��D�H�D���D���D��D�H�D���D���D��D�H�D���D���D��D�H�D���D���D��D�H�D���D���D��D�H�D���D���D�)D�L)D���D���D��D�H�D���D���D��D�H�D���D���D��D�H�D���D���D��D�H�D���D���D��D�H�D���D���D��D�H�D���D���D��D�H�D���D���D��D�H�D���D���D��D�H�D���D���D��D�H�D���D���D��D�H�D���D���D��D�H�D���D���D��D�H�D���D���D��D�H�D���D���D��D�H�D���D���D��D�H�D���D���D��D�H�D���D���D��D�H�D���D���D��D�H�D���D���D��D�H�D���D���D��D�H�D���D���D��D�H�D���D���D��D�H�D���D���D��D�H�D���D���D��D�H�D���D���D��D�H�D���D���D��D�H�D���D���D��D�H�D���D���D��D�H�D���D���D��D�H�D���D���D��D�H�D���D���D��D�H�D���D���D��D�H�D���D���D��D�H�D���D���D��D�H�D���D���D��D�H�D���D���D��D�H�D���D���D��D�H�D���D���D��D�H�D���D���D��D�H�D���D���D��D�H�D���D���D��D�H�D���D���D�)D�H�D���D���D��D�H�D���D���D��D�H�D���D���D��D�H�D���D���D��D�H�D���D���D��D�H�D�D���D��D�H�DÈ�D���D��D�H�DĈ�D���D��D�H�Dň�D���D��D�H�Dƈ�D���D��D�H�Dǈ�D���D��D�H�DȈ�D���D��D�H�DɈ�D���D��D�H�Dʈ�D���D��D�H�Dˈ�D���D��D�H�D̈�D���D��D�H�D͈�D���D��D�H�DΈ�D���D��D�H�Dψ�D���D��D�H�DЈ�D���D��D�H�Dш�D���D��D�H�D҈�D���D��D�H�Dӈ�D���D��D�H�DԈ�D���D��D�H�DՈ�D���D��D�H�Dֈ�D���D��D�H�D׈�D���D��D�H�D؈�D���D��D�H�Dو�D���D��D�H�Dڈ�D���D��D�H�Dۈ�D���D��D�H�D܈�D���D��D�H�D݈�D���D��D�H�Dވ�D���D��D�H�D߈�D���D��D�H�D���D���D�)D�H�D��D���D��D�H�D��D���D��D�H�D��D���D��D�H�D��D���D��D�H�D��D���D��D�H�D��D���D��D�H�D��D���D��D�H�D��D���D��D�H�D��D���D��D�H�D��D���D��D�H�D��D���D��D�H�D��D���D��D�H�D��D���D��D�H�D��D���D��D�H�D��D���D��D�H�D���D���D��D�H�D��D���D��D�H�D��D���D��D�H�D��D���D��D�H�D��D���D��D�H�D���D���D��D�H�D���D���D��D�H�D���D��)D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�7LA�7LA�7LA�7LA�9XA�7LA�;dA�9XA�=qA�A�A�?}A�5?A�+A��A�A���A��TA��A���A�I�A��RA�E�A�?}A��DA�G�A�/A��;A�E�A��A�x�A�A�33A���A�ȴA�x�A�{A�ĜA���A�K�A�bNA�S�A�?}A��A���A�5?A�ȴA�x�A���A�XA��A�A�ȴA��A��9A�  A���A�oA��
A�XA�JA��7A��A�33A�|�A�VA��mA�n�A�=qA��/A��7A�E�A���A�`BA���A��hA�5?A��RA���A���A�XA�Q�A�A��9A���A�VA�?}A�r�A�z�A�&�A�{A���A�+A�`BA�A���A�K�A�~�A��!A���A�l�A�-A��hA}�wA|A�A{/AzĜAx�jAuAsx�Ar  Ap�Am�PAj^5Ag33AfQ�AehsAc�A`^5A]/A[�AZAU�#AT$�AR�DAQ33AP��AN~�AK�TAJbAGG�AD�!AC;dA@�A>�9A=��A;�TA:-A7��A5�hA4��A4��A4JA0�/A0$�A/�^A/;dA.�`A.�A.��A.�!A.�DA,�\A+A+�A+hsA*�!A)�#A)
=A(=qA( �A'�A&z�A%��A%\)A#�^A!p�A {A�Ar�A�^A$�A�9A�
A(�A�/A�!AhsA��A��A��A5?A(�AG�A��A�mA�hA7LA�jA��A1'A�-A?}A
�DA	��A	��A	33A	%A�A�RAQ�A1'A��A+Ax�AM�A�AAbNA\)A"�A
=A �A J@�E�@��^@�/@�Z@���@�Q�@���@�@�v�@�Ĝ@�-@��`@띲@�5?@��;@�~�@�V@�1@�;d@���@�G�@�9@�\)@���@ݑh@۶F@�hs@؛�@�j@ָR@Ԭ@�o@ҟ�@�5?@ѩ�@�7L@�K�@��@̋D@��m@�v�@�%@ǶF@�
=@�M�@��@őh@�O�@�r�@�"�@�J@�x�@���@�9X@���@��@�I�@��;@�33@��!@��#@�`B@�bN@�1@�l�@�^5@���@�X@���@�I�@��@�\)@��R@�@���@�b@�ƨ@�@�@�x�@�?}@�j@�\)@�v�@���@�7L@��9@��m@��!@�@��@�V@�/@���@��u@���@�;d@��@��@�
=@���@��\@��\@�v�@���@�"�@��@�5?@��`@�O�@��@��9@�I�@���@���@�C�@�33@���@�$�@���@��h@�/@��j@�r�@�b@���@��@�o@��!@�n�@��T@��^@��@��T@��h@�?}@��D@�1@��;@��F@��@�|�@��@�K�@���@���@�V@�5?@�5?@�$�@��@�@�p�@�X@�G�@��@��j@�bN@��;@�\)@���@�$�@���@��#@���@��-@���@���@�hs@�O�@�&�@�V@��`@��@�I�@�(�@��
@��P@�t�@�dZ@��@���@���@�~�@�=q@�{@���@�@���@���@��@�`B@�G�@�?}@��@��j@��@�z�@�(�@�1@�  @�ƨ@�t�@�C�@��y@��+@�5?@��@��h@�hs@�&�@�V@�%@�%@���@���@�j@��@��@�\)@�C�@�o@��@���@�{@��T@��h@�hs@�?}@���@�Ĝ@��u@�9X@�  @�l�@�K�@�+@��@��!@��\@�ff@�M�@�M�@�-@���@��@��@���@�7L@�7L@��@���@��D@�r�@�Q�@�b@�  @��@���@��@�dZ@�C�@�"�@�
=@�@�@��y@��!@��\@��+@�v�@�ff@�V@�=q@��@��@��T@�p�@�X@�O�@�&�@�Ĝ@��u@�9X@�1@�P@\)@;d@~��@~ȴ@~ff@~5?@}�@}p�@}�@|j@{�
@{33@{o@z�H@z��@z�\@z=q@zJ@yX@x�9@xQ�@xA�@w�@w�@vȴ@v��@v5?@u�T@u�-@u��@u/@tj@s�F@sS�@r�\@q�#@q��@q��@q�^@q��@qhs@q&�@q�@pĜ@p  @o
=@nv�@n$�@n{@n@m�T@m��@mV@l�j@l�D@lz�@lz�@l(�@kC�@j�H@j��@j��@j�!@j�\@j~�@j~�@jM�@j�@i�#@i�7@iX@i7L@hbN@h  @g��@gK�@g+@g
=@f�y@f�R@f�+@fff@f5?@e��@ep�@e?}@d�/@dz�@dj@dZ@d1@c�
@cƨ@c�F@c��@c�@ct�@cdZ@c@b�\@b�@a��@aX@a%@a%@`��@`Ĝ@`r�@_��@_��@_|�@^��@^��@^V@^5?@^$�@]��@]�@\�/@\��@\�j@\�D@\Z@\�@[ƨ@[t�@[@Z=q@Y7L@XĜ@X��@Y�@Y�@X��@X��@XbN@XA�@X  @WK�@V�R@V�R@V�R@U�h@T��@T�D@T�j@T9X@S�
@So@Rn�@R=q@Q�@Q��@Q��@P��@PbN@O��@O��@Ol�@O;d@O�@O
=@N�@N�+@NV@NE�@M�@M��@Mp�@M?}@L�@L�@Lz�@L�@K�@Ko@J�!@J-@I�@I��@I�@HĜ@H��@HbN@H1'@G�@G��@G�@Fȴ@Fff@E�@E�h@E�h@Ep�@E?}@D��@D��@Dj@C�m@C�F@C�@CS�@C"�@C@B�\@BM�@A�#@A&�@@Ĝ@@Ĝ@@�@@Q�@@  @?�w@?l�@?�@>�@>��@>v�@>V@=�@=`B@<�@<�D@<9X@<�@;��@;�m@;�F@;S�@;o@:��@:�!@:n�@:=q@9�@9��@9X@97L@9�@9%@8��@8Ĝ@8�u@8Q�@8 �@7�;@7�@7K�@6��@6V@5�@5�h@4�/@4z�@4I�@4(�@4(�@41@3��@3��@333@3@2�H@2��@2=q@2-@2J@1�^@1�7@1&�@0��@0bN@0 �@0 �@0b@/�;@/�@/��@/�P@/�P@/|�@/l�@/K�@/;d@/
=@/
=@.�R@.E�@.@-��@-@-��@-`B@,�/@,�D@,(�@+��@+�
@+��@+t�@+C�@+C�@+"�@+o@+o@*�@*�@*�@*�@*�H@*��@*M�@*J@*J@)�#@)x�@)%@(�u@(bN@(1'@(b@'�;@'�;@'��@'�@'��@'�P@'�P@'l�@'K�@'+@&�y@&�R@&v�@&5?@&@%�T@%�T@%@%��@%��@%�-@%p�@%V@$�@$��@$�D@$z�@$z�@$z�@#�m@#�@#dZ@#S�@#33@"�H@"�\@"^5@"-@!�@!��@!��@!X@!7L@!7L@!7L@!7L@!G�@!&�@!%@ �`@ Ĝ@ A�@�@�w@K�@�@��@�+@v�@E�@�-@`B@O�@?}@�@��@�@��@��@9X@�
@t�@33@@��@~�@=q@�@��@��@��@��@�7@G�@7L@%@Ĝ@bN@A�@�@�w@�@�@�@�@��@��@l�@�@�y@ȴ@�+@E�@$�@{@�@�T@@�@O�@O�@?}@?}@V@�/@�D@I�@(�@�@�@��@��@��@��@S�@"�@��@�\@n�@M�@J@�@�#@��@�^@x�@7L@��@�`@�9@�@A�@ �@  @�;@��@�w@|�@l�@;d@�@��@��@v�@E�@@�@�T@��@��@�h@V@��@�@�@��@�D@j@(�@�m@ƨ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�7LA�7LA�7LA�7LA�9XA�7LA�;dA�9XA�=qA�A�A�?}A�5?A�+A��A�A���A��TA��A���A�I�A��RA�E�A�?}A��DA�G�A�/A��;A�E�A��A�x�A�A�33A���A�ȴA�x�A�{A�ĜA���A�K�A�bNA�S�A�?}A��A���A�5?A�ȴA�x�A���A�XA��A�A�ȴA��A��9A�  A���A�oA��
A�XA�JA��7A��A�33A�|�A�VA��mA�n�A�=qA��/A��7A�E�A���A�`BA���A��hA�5?A��RA���A���A�XA�Q�A�A��9A���A�VA�?}A�r�A�z�A�&�A�{A���A�+A�`BA�A���A�K�A�~�A��!A���A�l�A�-A��hA}�wA|A�A{/AzĜAx�jAuAsx�Ar  Ap�Am�PAj^5Ag33AfQ�AehsAc�A`^5A]/A[�AZAU�#AT$�AR�DAQ33AP��AN~�AK�TAJbAGG�AD�!AC;dA@�A>�9A=��A;�TA:-A7��A5�hA4��A4��A4JA0�/A0$�A/�^A/;dA.�`A.�A.��A.�!A.�DA,�\A+A+�A+hsA*�!A)�#A)
=A(=qA( �A'�A&z�A%��A%\)A#�^A!p�A {A�Ar�A�^A$�A�9A�
A(�A�/A�!AhsA��A��A��A5?A(�AG�A��A�mA�hA7LA�jA��A1'A�-A?}A
�DA	��A	��A	33A	%A�A�RAQ�A1'A��A+Ax�AM�A�AAbNA\)A"�A
=A �A J@�E�@��^@�/@�Z@���@�Q�@���@�@�v�@�Ĝ@�-@��`@띲@�5?@��;@�~�@�V@�1@�;d@���@�G�@�9@�\)@���@ݑh@۶F@�hs@؛�@�j@ָR@Ԭ@�o@ҟ�@�5?@ѩ�@�7L@�K�@��@̋D@��m@�v�@�%@ǶF@�
=@�M�@��@őh@�O�@�r�@�"�@�J@�x�@���@�9X@���@��@�I�@��;@�33@��!@��#@�`B@�bN@�1@�l�@�^5@���@�X@���@�I�@��@�\)@��R@�@���@�b@�ƨ@�@�@�x�@�?}@�j@�\)@�v�@���@�7L@��9@��m@��!@�@��@�V@�/@���@��u@���@�;d@��@��@�
=@���@��\@��\@�v�@���@�"�@��@�5?@��`@�O�@��@��9@�I�@���@���@�C�@�33@���@�$�@���@��h@�/@��j@�r�@�b@���@��@�o@��!@�n�@��T@��^@��@��T@��h@�?}@��D@�1@��;@��F@��@�|�@��@�K�@���@���@�V@�5?@�5?@�$�@��@�@�p�@�X@�G�@��@��j@�bN@��;@�\)@���@�$�@���@��#@���@��-@���@���@�hs@�O�@�&�@�V@��`@��@�I�@�(�@��
@��P@�t�@�dZ@��@���@���@�~�@�=q@�{@���@�@���@���@��@�`B@�G�@�?}@��@��j@��@�z�@�(�@�1@�  @�ƨ@�t�@�C�@��y@��+@�5?@��@��h@�hs@�&�@�V@�%@�%@���@���@�j@��@��@�\)@�C�@�o@��@���@�{@��T@��h@�hs@�?}@���@�Ĝ@��u@�9X@�  @�l�@�K�@�+@��@��!@��\@�ff@�M�@�M�@�-@���@��@��@���@�7L@�7L@��@���@��D@�r�@�Q�@�b@�  @��@���@��@�dZ@�C�@�"�@�
=@�@�@��y@��!@��\@��+@�v�@�ff@�V@�=q@��@��@��T@�p�@�X@�O�@�&�@�Ĝ@��u@�9X@�1@�P@\)@;d@~��@~ȴ@~ff@~5?@}�@}p�@}�@|j@{�
@{33@{o@z�H@z��@z�\@z=q@zJ@yX@x�9@xQ�@xA�@w�@w�@vȴ@v��@v5?@u�T@u�-@u��@u/@tj@s�F@sS�@r�\@q�#@q��@q��@q�^@q��@qhs@q&�@q�@pĜ@p  @o
=@nv�@n$�@n{@n@m�T@m��@mV@l�j@l�D@lz�@lz�@l(�@kC�@j�H@j��@j��@j�!@j�\@j~�@j~�@jM�@j�@i�#@i�7@iX@i7L@hbN@h  @g��@gK�@g+@g
=@f�y@f�R@f�+@fff@f5?@e��@ep�@e?}@d�/@dz�@dj@dZ@d1@c�
@cƨ@c�F@c��@c�@ct�@cdZ@c@b�\@b�@a��@aX@a%@a%@`��@`Ĝ@`r�@_��@_��@_|�@^��@^��@^V@^5?@^$�@]��@]�@\�/@\��@\�j@\�D@\Z@\�@[ƨ@[t�@[@Z=q@Y7L@XĜ@X��@Y�@Y�@X��@X��@XbN@XA�@X  @WK�@V�R@V�R@V�R@U�h@T��@T�D@T�j@T9X@S�
@So@Rn�@R=q@Q�@Q��@Q��@P��@PbN@O��@O��@Ol�@O;d@O�@O
=@N�@N�+@NV@NE�@M�@M��@Mp�@M?}@L�@L�@Lz�@L�@K�@Ko@J�!@J-@I�@I��@I�@HĜ@H��@HbN@H1'@G�@G��@G�@Fȴ@Fff@E�@E�h@E�h@Ep�@E?}@D��@D��@Dj@C�m@C�F@C�@CS�@C"�@C@B�\@BM�@A�#@A&�@@Ĝ@@Ĝ@@�@@Q�@@  @?�w@?l�@?�@>�@>��@>v�@>V@=�@=`B@<�@<�D@<9X@<�@;��@;�m@;�F@;S�@;o@:��@:�!@:n�@:=q@9�@9��@9X@97L@9�@9%@8��@8Ĝ@8�u@8Q�@8 �@7�;@7�@7K�@6��@6V@5�@5�h@4�/@4z�@4I�@4(�@4(�@41@3��@3��@333@3@2�H@2��@2=q@2-@2J@1�^@1�7@1&�@0��@0bN@0 �@0 �@0b@/�;@/�@/��@/�P@/�P@/|�@/l�@/K�@/;d@/
=@/
=@.�R@.E�@.@-��@-@-��@-`B@,�/@,�D@,(�@+��@+�
@+��@+t�@+C�@+C�@+"�@+o@+o@*�@*�@*�@*�@*�H@*��@*M�@*J@*J@)�#@)x�@)%@(�u@(bN@(1'@(b@'�;@'�;@'��@'�@'��@'�P@'�P@'l�@'K�@'+@&�y@&�R@&v�@&5?@&@%�T@%�T@%@%��@%��@%�-@%p�@%V@$�@$��@$�D@$z�@$z�@$z�@#�m@#�@#dZ@#S�@#33@"�H@"�\@"^5@"-@!�@!��@!��@!X@!7L@!7L@!7L@!7L@!G�@!&�@!%@ �`@ Ĝ@ A�@�@�w@K�@�@��@�+@v�@E�@�-@`B@O�@?}@�@��@�@��@��@9X@�
@t�@33@@��@~�@=q@�@��@��@��@��@�7@G�@7L@%@Ĝ@bN@A�@�@�w@�@�@�@�@��@��@l�@�@�y@ȴ@�+@E�@$�@{@�@�T@@�@O�@O�@?}@?}@V@�/@�D@I�@(�@�@�@��@��@��@��@S�@"�@��@�\@n�@M�@J@�@�#@��@�^@x�@7L@��@�`@�9@�@A�@ �@  @�;@��@�w@|�@l�@;d@�@��@��@v�@E�@@�@�T@��@��@�h@V@��@�@�@��@�D@j@(�@�m@ƨ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�-B�LB��B��BBDBB�BB1B\B�B�B%�B+B-B0!B49B;dBD�BI�BN�BdZB� B�VB�bB�hB�{B��B��B��B�uB�\B�=B�1B�JB�bB�oB�VBy�BdZBgmBffBQ�BF�B<jB.B&�B)�B,B\BPB	7BB��B�B�sB�`B�`B�#B��B��B�wB��B�=Bm�BW
BK�BJ�B$�B�B
��B
�B
�mB
ƨB
��B
x�B
\)B
N�B
H�B
/B
hB	��B	��B	��B	�B	�B	ǮB	�qB	�XB	�B	�oB	�B	y�B	gmB	W
B	B�B	'�B	�B	�B		7B�B�B��B�jB��B�bB�+B� Bz�Bq�BcTB]/BR�BE�B:^B5?B-B(�B"�B�B�B�B�B�B�BoBoB�B�B�B�B�B�B&�B1'B9XB>wB@�B?}BA�BH�BO�BT�BW
BW
B]/B\)BS�BM�BN�BK�BJ�BK�BN�BK�BN�BL�BG�BG�BF�BE�BN�BS�BS�BYBT�BR�BO�BQ�BW
BYBYBW
BR�BS�BT�BQ�BO�BN�BN�BN�BN�BO�BN�BN�BR�BO�BG�BD�BI�BE�B=qB>wB>wB>wB>wB<jB:^B:^B8RB8RB49B49B1'B0!B/B.B,B,B+B,B,B)�B(�B)�B+B)�B+B+B+B,B-B,B+B)�B-B0!B2-B1'B2-B33B33B7LB9XB8RB:^B:^B=qB?}B@�BA�BA�BA�BA�BB�BI�BL�BL�BK�BL�BT�B^5BaHBaHBbNBdZBhsBiyBq�Br�Bt�Bu�Bx�By�Bz�B{�B|�B� B�B�B�B�B�+B�1B�VB�hB�oB��B��B��B��B��B��B��B��B��B��B��B��B�B��B�B�FB��BŢBɺB��B��B��B��B��B�)B�/B�;B�NB�mB�B�B�B�B�B��B��B��B	  B	B	%B		7B	JB	VB	bB	oB	oB	�B	�B	�B	�B	!�B	&�B	'�B	)�B	+B	,B	,B	-B	0!B	33B	6FB	9XB	<jB	?}B	D�B	H�B	I�B	J�B	K�B	M�B	P�B	S�B	T�B	VB	YB	[#B	^5B	bNB	gmB	jB	o�B	p�B	q�B	r�B	t�B	u�B	v�B	x�B	y�B	{�B	|�B	~�B	�B	�B	�B	�+B	�1B	�7B	�7B	�DB	�JB	�VB	�\B	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�9B	�FB	�LB	�RB	�^B	�dB	�dB	�dB	�dB	�wB	�}B	B	ĜB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�)B	�)B	�/B	�5B	�BB	�BB	�HB	�NB	�NB	�NB	�TB	�TB	�TB	�TB	�`B	�`B	�`B	�mB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
+B
1B
	7B
	7B

=B

=B
DB
JB
JB
JB
JB
PB
PB
PB
PB
PB
PB
PB
VB
\B
\B
\B
\B
\B
bB
bB
bB
bB
bB
bB
hB
hB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
!�B
!�B
#�B
#�B
$�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
&�B
&�B
(�B
)�B
+B
)�B
)�B
)�B
+B
)�B
)�B
+B
+B
,B
+B
+B
+B
+B
+B
+B
+B
,B
-B
-B
.B
.B
.B
/B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
0!B
2-B
33B
33B
49B
49B
49B
5?B
5?B
6FB
6FB
6FB
6FB
7LB
8RB
8RB
8RB
9XB
8RB
8RB
9XB
9XB
:^B
:^B
:^B
;dB
;dB
;dB
<jB
=qB
<jB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
@�B
@�B
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
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
F�B
F�B
F�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
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
N�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
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
T�B
T�B
T�B
T�B
VB
T�B
T�B
VB
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
XB
XB
XB
XB
XB
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
[#B
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
cTB
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
k�B
k�B
k�B
k�B
l�B
l�B
l�B
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
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B��B��B��B��B��B��B��B��B��B��B��B� B�B�B�2BʦB��BB)BB�BBBBBB�B%�B*�B,�B0B4B;JBD�BI�BN�Bd@B�B�<B�HB�NB�aB�gB�mB�mB�[B�BB�#B�B�0B�HB�TB�<By�Bd@BgRBfLBQ�BF�B<PB-�B&�B)�B+�BBB6B	B�B��B�wB�XB�FB�FB�	B��B��B�]B��B�#BmwBV�BK�BJ�B$�ByB
��B
�kB
�RB
ƎB
��B
x�B
\B
N�B
H�B
/ B
NB	��B	��B	��B	�wB	��B	ǔB	�VB	�>B	��B	�TB	��B	y�B	gRB	V�B	BuB	'�B	�B	mB		B�B��BʦB�PB��B�HB�B�Bz�Bq�Bc:B]BR�BE�B:*B5%B,�B(�B"�B�B�B�BsB�BBTBTBBBqB�B�B�B&�B1B9>B>]B@OB?cBAUBH�BO�BT�BV�BV�B]B\BS�BM�BN�BK�BJ�BK�BN�BK�BN�BL�BG�BG�BFtBE�BN�BS�BS�BX�BT�BR�BO�BQ�BV�BX�BX�BV�BR�BS�BT�BQ�BO�BN�BN�BN�BN�BO�BN�BN�BR�BO�BGzBDgBI�BE�B=<B>]B>BB>]B>]B<6B:DB:*B8B8B4B4B1B0B/ B-�B+�B+�B*�B+�B+�B)�B(�B)�B*�B)�B*�B*�B*�B+�B,�B+�B*�B)�B,�B0B2B1B1�B2�B3B72B9>B88B:*B:DB=VB?cB@iBAoBAoBAUBAoBBuBI�BL�BL�BK�BL�BT�B^BaBa-Bb4Bd@BhXBiDBq�Br�Bt�Bu�Bx�By�Bz�B{�B|�B�B��B��B��B�B�B��B�<B�NB�TB�SB�B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�OB�mBɆBʦBʦB̘BбB��B�B�B�B�4B�RB�kB�wB�B�B�B��B��B��B��B	�B	B		B	B	"B	HB	TB	TB	mB	_B	�B	�B	!�B	&�B	'�B	)�B	*�B	+�B	+�B	,�B	/�B	2�B	6B	9>B	<6B	?HB	D�B	H�B	I�B	J�B	K�B	M�B	P�B	S�B	T�B	U�B	X�B	Z�B	^B	bB	gRB	jeB	o�B	p�B	q�B	r|B	t�B	u�B	v�B	x�B	y�B	{�B	|�B	~�B	��B	��B	��B	�B	��B	�B	�B	�)B	�B	�<B	�BB	�TB	�[B	�aB	�sB	�sB	�_B	�B	��B	�qB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�2B	�8B	�DB	�0B	�JB	�JB	�0B	�BB	�HB	�uB	�gB	�tB	ǔB	ȚB	ɠB	ˬB	ΥB	ϫB	ѷB	��B	ҽB	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	�B	�B	�4B	�B	�4B	�:B	� B	�:B	�:B	�,B	�FB	�,B	�RB	�RB	�RB	�>B	�_B	�KB	�eB	�KB	�KB	�kB	�kB	�qB	�qB	�WB	�qB	�]B	��B	�vB	�oB	�B	�B	�B	�B	�|B	�B	�|B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
B
�B
�B
B
B
�B
B
B
�B
	B
	B

#B

	B
)B
0B
0B
B
0B
B
B
B
6B
B
6B
6B
"B
(B
(B
BB
(B
BB
.B
HB
HB
.B
HB
.B
4B
NB
:B
TB
:B
:B
TB
:B
TB
TB
TB
:B
:B
TB
aB
FB
MB
mB
mB
mB
SB
SB
mB
sB
sB
YB
YB
YB
yB
_B
_B
yB
B
B
eB
kB
�B
�B
kB
�B
�B
�B
�B
�B
xB
xB
�B
�B
�B
~B
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
!�B
!�B
"�B
"�B
"�B
!�B
!�B
#�B
#�B
$�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
&�B
&�B
(�B
)�B
*�B
)�B
)�B
)�B
*�B
)�B
)�B
*�B
*�B
+�B
*�B
*�B
*�B
*�B
*�B
*�B
*�B
+�B
,�B
,�B
-�B
-�B
-�B
/ B
/ B
/ B
/ B
/�B
/�B
0B
0B
/�B
/�B
1�B
2�B
3B
4B
4B
4B
5%B
5%B
6+B
6B
6B
6+B
7B
88B
88B
88B
9>B
88B
88B
9>B
9>B
:*B
:*B
:*B
;0B
;JB
;JB
<PB
=VB
<PB
=<B
=<B
=VB
=VB
>]B
>BB
>]B
?HB
?cB
?cB
?HB
@OB
@iB
AoB
AoB
AUB
AUB
AoB
AoB
BuB
BuB
B[B
BuB
B[B
C{B
C{B
CaB
C{B
C{B
CaB
C{B
D�B
D�B
D�B
D�B
D�B
DgB
DgB
EmB
FtB
F�B
FtB
GzB
H�B
H�B
H�B
H�B
H�B
H�B
H�B
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
N�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
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
T�B
T�B
T�B
T�B
U�B
T�B
T�B
U�B
T�B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
V�B
V�B
V�B
V�B
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
W�B
W�B
W�B
X�B
X�B
X�B
X�B
X�B
X�B
ZB
ZB
ZB
ZB
ZB
[	B
Z�B
[	B
[	B
Z�B
Z�B
Z�B
[	B
[	B
Z�B
[	B
\B
\B
\B
\�B
\�B
\�B
]B
^B
^B
^B
^B
_B
_!B
_B
_!B
_B
_B
_B
`B
`'B
`'B
`'B
a-B
aB
a-B
a-B
b4B
b4B
bB
bB
bB
b4B
c B
bB
c:B
c:B
c:B
c B
d@B
d@B
d&B
d&B
d@B
d@B
d@B
d&B
d&B
eFB
eFB
eFB
eFB
eFB
fLB
f2B
fLB
f2B
fLB
fLB
fLB
g8B
f2B
gRB
gRB
gRB
g8B
g8B
gRB
hXB
hXB
h>B
hXB
hXB
h>B
h>B
i_B
i_B
iDB
jKB
jeB
jeB
jeB
jeB
jKB
jeB
kQB
kQB
kkB
kkB
lqB
lqB
lWB
mwB
mwB
m]B
mwB
mwB
m]B
n}B
n}B
ncB
ncB
n}B
ncB
oiB
o�B
o�B
oiB
o�B
o�B
o�B
poB
p�B
p�B
p�B
p�B
p�B
poB
q�B
qvB
qv11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.28(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201804230048122018042300481220180423004812201804261732092018042617320920180426173209JA  ARFMdecpA19c                                                                20180418183517  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180418093534  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180418093534  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180418093535  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180418093535  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180418093535  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180418093535  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180418093535  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180418093536  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180418093536                      G�O�G�O�G�O�                JA  ARUP                                                                        20180418095616                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180418154456  CV  JULD            G�O�G�O�F��V                JM  ARGQJMQC2.0                                                                 20180418154456  CV  JULD_LOCATION   G�O�G�O�F��x                JM  ARGQJMQC2.0                                                                 20180418154456  CV  LATITUDE        G�O�G�O�A���                JM  ARGQJMQC2.0                                                                 20180418154456  CV  LONGITUDE       G�O�G�O��8�                JM  ARCAJMQC2.0                                                                 20180422154812  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180422154812  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180426083209  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021523                      G�O�G�O�G�O�                