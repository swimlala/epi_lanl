CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-11-09T12:37:30Z creation;2019-11-09T12:37:35Z conversion to V3.1;2022-11-21T05:28:03Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7,   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7<   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7@   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7D   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7T   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7d   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7t   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7|   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8,   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    80   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    84   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     88   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8X   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8\   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8`   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  M�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  U�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  W�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  _�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  i|   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  kx   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  s`   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  u\   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  }D   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �,   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �$   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �(   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �,   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �0   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �4   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �t   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20191109123730  20221123111508  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_191                     2C  D   NAVIS_A                         0397                            ARGO 011514                     863 @��W; 1   @�꛻���@;��쿱[�dg��	k�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BO��BX  B`ffBh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� DkfDk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}�D}Ff11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�p�@�=qA�A!�AA�Aa�A��\A��\A��\A��\A��\AЏ\A��\A��\B G�BG�BG�BG�B G�B(G�B0G�B8G�B@G�BHG�BO�HBXG�B`�BhG�BpG�BxG�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�W
B�#�B�#�B�#�C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D {D �{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D	{D	�{D
{D
�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D {D �{D!{D!�{D"{D"�{D#{D#�{D${D$�{D%{D%�{D&{D&�{D'{D'�{D({D(�{D){D)�{D*{D*�{D+{D+�{D,{D,�{D-{D-�{D.{D.�{D/{D/�{D0{D0�{D1{D1�{D2{D2�{D3{D3�{D4{D4�{D5{D5�{D6{D6�{D7{D7�{D8{D8�{D9{D9�{D:{D:�{D;{D;�{D<{D<�{D={D=�{D>{D>�{D?{D?�{D@{D@�{DA{DA�{DB{DB�{DC{DC�{DD{DD�{DE{DE�{DF{DF�{DG{DG�{DH{DH�{DI{DI�{DJ{DJ�{DK{DK�{DL{DL�{DM{DM�{DN{DN�{DO{DO�{DP{DP�{DQ{DQ�{DR{DR�{DS{DS�{DT{DT�{DU{DU�{DV{DV�{DW{DW�{DX{DX�{DY{DY�{DZ{DZ�{D[{D[�{D\{D\�{D]{D]�{D^{D^�{D_{D_�{D`{D`�{Da{Da�{Db{Db�{Dc{Dc�{Dd{Dd�{De{De�{Df{Df�{Dg{Dg�{Dh{Dh�{Di{Di�{Dj{Dj�{Dk
�Dk�{Dl{Dl�{Dm{Dm�{Dn{Dn�{Do{Do�{Dp{Dp�{Dq{Dq�{Dr{Dr�{Ds{Ds�{Dt{Dt�{Du{Du�{Dv{Dv�{Dw{Dw�{Dx{Dx�{Dy{Dy�{Dz{Dz�{D{{D{�{D|{D|�{D}HD}J�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�O�A�O�A�VA�ZA�VA�VA�XA�VA�XA�VA�Q�A�O�A�M�A�33A�JA�  A��yA��#A���AǗ�A�|�A��A�E�A��A�`BA�^5A�ƨA��A���A���A���A�G�A��/A�C�A��PA��`A�ZA���A�I�A��A���A��A�A�%A�jA��A�ĜA�5?A�A��RA���A��A��`A���A���A�A�A���A�G�A���A��DA�jA���A�x�A�bNA���A�r�A��+A�33A�7LA��A���A�E�A�z�A�ZA��yA�\)A���A�ZA�A�jA���A+A~��A}�^A}?}A{|�AydZAx�jAwK�Aux�At�At$�Ar�/Ap��Ap1Aot�AoVAn�AlAj=qAiƨAi`BAh��Ah^5Af�9AeC�Ad��Adr�Ad1'Acp�Ab��Ab^5Aa?}A_��A^^5A]��A\jA[K�A[AZ�AY��AX�`AW&�AU�7ATȴAS��AR��ARr�AR-AQ��AQhsAP�AP �AN�AM�hALz�AK�-AK`BAKO�AK;dAKAI|�AG��AGhsAGC�AF�AF�jAF(�AEoAD �AB�yAAO�A@�+A?��A?x�A>��A=�TA=��A=�A<z�A:�+A9�^A8�`A7��A5�;A4�`A4��A4  A3/A2�9A1��A0�A/�A-�PA-?}A,VA*ZA)/A(9XA&��A%�TA%�7A%C�A${A#;dA"�HA"��A"=qA"�A!�TA!A M�A��A�uA^5A{A�;A�^A�AjA��A1'A��A+AffA��Av�AE�A�#A7LA��A9XAO�A(�A5?AA�uAE�A �AA�A��A�7AVA�DAVA�
A`BA
��A	��A�!A�;A�7A7LA��AƨAI�AS�A~�AM�A  A�7A ��A �\A ~�@�t�@��\@�Ĝ@���@��@��^@��P@�9X@�ƨ@�33@�@�9X@�p�@��y@�{@�7L@蛦@�@��m@���@��@�Z@⟾@�?}@�r�@�+@ݙ�@�A�@ە�@�K�@��@��@�@�o@��y@�-@�G�@ԣ�@�|�@�$�@��@�j@�t�@�@���@��m@˅@�K�@���@ɺ^@�/@��
@�@��@Ə\@�J@���@Å@�v�@�?}@�1'@���@���@��9@�E�@�@��h@�p�@�7L@��j@��@��
@�C�@��#@�?}@�bN@���@�C�@��+@��7@��`@��m@�"�@���@���@�n�@�V@�V@�M�@�V@�V@�J@�x�@�9X@��;@���@�5?@��@��@��@���@��R@��@��7@���@��@��;@��F@���@���@��@���@���@�t�@�"�@�M�@�p�@��D@��w@�
=@�hs@��@��;@��@�@�V@���@��@�X@��@���@��`@�Ĝ@��9@��u@�z�@�1@�"�@�J@��@��7@�Ĝ@���@��u@�bN@�(�@� �@�|�@�33@�@��y@��!@�v�@��@�p�@��@�%@�Ĝ@��u@�Q�@� �@��m@�S�@��@�M�@�5?@��#@��@�A�@��w@�S�@���@�n�@��#@��7@�?}@�7L@��@��@� �@���@�t�@�@���@�E�@�{@��^@�O�@���@���@���@���@���@��u@�(�@�w@~�R@~E�@~$�@}��@}`B@}?}@}�@|�@|��@|�@|�j@{��@z�\@zM�@z�@y�@y�^@y��@y��@x��@x�u@x�@x1'@w|�@w�@v�@vȴ@v�R@v�R@v�R@v�+@vV@vE�@u��@t��@tz�@t9X@s�@sC�@r��@rn�@rn�@rM�@qG�@p��@p�u@p�@pr�@pbN@pbN@pbN@pQ�@p �@pb@o��@ol�@o\)@n��@o
=@n��@nE�@n{@n@m�@m�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�O�A�O�A�VA�ZA�VA�VA�XA�VA�XA�VA�Q�A�O�A�M�A�33A�JA�  A��yA��#A���AǗ�A�|�A��A�E�A��A�`BA�^5A�ƨA��A���A���A���A�G�A��/A�C�A��PA��`A�ZA���A�I�A��A���A��A�A�%A�jA��A�ĜA�5?A�A��RA���A��A��`A���A���A�A�A���A�G�A���A��DA�jA���A�x�A�bNA���A�r�A��+A�33A�7LA��A���A�E�A�z�A�ZA��yA�\)A���A�ZA�A�jA���A+A~��A}�^A}?}A{|�AydZAx�jAwK�Aux�At�At$�Ar�/Ap��Ap1Aot�AoVAn�AlAj=qAiƨAi`BAh��Ah^5Af�9AeC�Ad��Adr�Ad1'Acp�Ab��Ab^5Aa?}A_��A^^5A]��A\jA[K�A[AZ�AY��AX�`AW&�AU�7ATȴAS��AR��ARr�AR-AQ��AQhsAP�AP �AN�AM�hALz�AK�-AK`BAKO�AK;dAKAI|�AG��AGhsAGC�AF�AF�jAF(�AEoAD �AB�yAAO�A@�+A?��A?x�A>��A=�TA=��A=�A<z�A:�+A9�^A8�`A7��A5�;A4�`A4��A4  A3/A2�9A1��A0�A/�A-�PA-?}A,VA*ZA)/A(9XA&��A%�TA%�7A%C�A${A#;dA"�HA"��A"=qA"�A!�TA!A M�A��A�uA^5A{A�;A�^A�AjA��A1'A��A+AffA��Av�AE�A�#A7LA��A9XAO�A(�A5?AA�uAE�A �AA�A��A�7AVA�DAVA�
A`BA
��A	��A�!A�;A�7A7LA��AƨAI�AS�A~�AM�A  A�7A ��A �\A ~�@�t�@��\@�Ĝ@���@��@��^@��P@�9X@�ƨ@�33@�@�9X@�p�@��y@�{@�7L@蛦@�@��m@���@��@�Z@⟾@�?}@�r�@�+@ݙ�@�A�@ە�@�K�@��@��@�@�o@��y@�-@�G�@ԣ�@�|�@�$�@��@�j@�t�@�@���@��m@˅@�K�@���@ɺ^@�/@��
@�@��@Ə\@�J@���@Å@�v�@�?}@�1'@���@���@��9@�E�@�@��h@�p�@�7L@��j@��@��
@�C�@��#@�?}@�bN@���@�C�@��+@��7@��`@��m@�"�@���@���@�n�@�V@�V@�M�@�V@�V@�J@�x�@�9X@��;@���@�5?@��@��@��@���@��R@��@��7@���@��@��;@��F@���@���@��@���@���@�t�@�"�@�M�@�p�@��D@��w@�
=@�hs@��@��;@��@�@�V@���@��@�X@��@���@��`@�Ĝ@��9@��u@�z�@�1@�"�@�J@��@��7@�Ĝ@���@��u@�bN@�(�@� �@�|�@�33@�@��y@��!@�v�@��@�p�@��@�%@�Ĝ@��u@�Q�@� �@��m@�S�@��@�M�@�5?@��#@��@�A�@��w@�S�@���@�n�@��#@��7@�?}@�7L@��@��@� �@���@�t�@�@���@�E�@�{@��^@�O�@���@���@���@���@���@��u@�(�@�w@~�R@~E�@~$�@}��@}`B@}?}@}�@|�@|��@|�@|�j@{��@z�\@zM�@z�@y�@y�^@y��@y��@x��@x�u@x�@x1'@w|�@w�@v�@vȴ@v�R@v�R@v�R@v�+@vV@vE�@u��@t��@tz�@t9X@s�@sC�@r��@rn�@rn�@rM�@qG�@p��@p�u@p�@pr�@pbN@pbN@pbN@pQ�@p �@pb@o��@ol�@o\)@n��@o
=@n��@nE�@n{@n@m�@m�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B%�B%�B%�B%�B$�B$�B$�B$�B$�B#�B#�B#�B#�B#�B$�B%�B%�B%�B&�B(�B&�B �B%B�fB�qB��B��B��B��B�+Bv�Bn�Bl�B]/BdZBbNBYBC�B&�B�B�BPB1B
=BPB{BuBbBbBPBB��B��B��B�B�B�ZB�/B��B�LB��B��B�BgmBcTBM�B8RB/B�BPB
��B
�B
�BB
��B
ǮB
��B
ĜB
ÖB
�wB
�9B
��B
�7B
�B
z�B
u�B
iyB
ZB
R�B
I�B
>wB
9XB
49B
,B
 �B
�B
�B
{B
\B
B	��B	�B	�B	�B	�yB	�HB	�B	��B	��B	��B	��B	��B	��B	ĜB	�^B	�'B	�B	��B	��B	��B	��B	��B	��B	�JB	�B	�B	|�B	y�B	x�B	w�B	v�B	s�B	p�B	k�B	dZB	aHB	]/B	ZB	YB	XB	W
B	T�B	L�B	B�B	?}B	>wB	;dB	9XB	5?B	0!B	)�B	#�B	�B	�B	�B	�B	�B	uB	hB	VB		7B��B��B�B�B�`B�TB�HB�BB�5B�#B�B��BĜB�}B�qB�dB�?B�!B�B��B��B��B��B��B�{B�uB�hB�bB�bB�bB�JB�7B�+B�B�B�B�B�B� B{�Bw�Bs�Bq�Bn�Bl�BhsBe`BdZBcTBaHB_;B]/B[#BW
BS�BQ�BP�BO�BO�BO�BN�BN�BM�BL�BK�BK�BJ�BI�BG�BE�BC�BA�B@�B?}B=qB;dB8RB7LB6FB5?B49B33B2-B2-B1'B0!B/B.B.B-B+B)�B)�B(�B'�B&�B%�B#�B"�B!�B!�B!�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B"�B!�B!�B!�B!�B!�B!�B!�B!�B!�B"�B#�B#�B#�B#�B#�B#�B$�B$�B'�B(�B(�B(�B(�B+B-B.B.B/B1'B49B6FB;dB;dB;dB;dB;dB<jB<jB=qB>wBB�BC�BF�BH�BI�BK�BM�BO�BQ�BR�BS�BT�BT�BT�BT�BVBT�BT�BVBW
BZBZBYB[#B\)B^5B`BBbNBe`BhsBhsBiyBl�Bl�Bl�Bo�Br�Br�Bt�Bt�Bz�B{�B� B�B�1B�=B�JB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�-B�?B�LB�LB�XB�XB�^B�jB�wB�wB�}B��B��BÖBŢBǮBǮBɺB��B��B��B��B��B��B�B�B�)B�BB�NB�ZB�fB�yB�B�B��B��B��B��B	  B	B	1B	VB	hB	uB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	#�B	'�B	)�B	)�B	,B	-B	.B	.B	1'B	33B	49B	5?B	9XB	?}B	A�B	A�B	B�B	B�B	B�B	B�B	E�B	F�B	F�B	G�B	I�B	K�B	L�B	M�B	M�B	N�B	N�B	O�B	Q�B	R�B	W
B	ZB	\)B	]/B	_;B	`BB	bNB	cTB	cTB	cTB	ffB	gmB	hsB	iyB	iyB	iyB	iyB	iyB	jB	k�B	l�B	n�B	r�B	t�B	u�B	w�B	{�B	�B	�B	�B	�B	�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B%�B%�B%�B%�B$�B$�B$�B$�B$�B#�B#�B#�B$B$&B$�B&B&B&2B'mB*B)�B'�B�B�B�B��B�`B��B��B�\B|�BpoBo�B_;Bf�BffB^�BHKB(�B!B=BpB	�BxBBMB�B BhB�B�B�2B�B�ZB�B��B�zB��B�pB��B�B�/B��Bi_Bf2BO�B9�B1�B"�B\B
��B
�B
�hB
��B
�B
�-B
�mB
ĶB
��B
�fB
�NB
�#B
�9B
|B
w�B
k�B
[WB
T�B
K�B
?}B
:xB
5�B
-�B
!�B
]B
EB
�B
 B
�B	��B	�9B	�[B	�}B	�kB	�B	خB	�aB	�[B	��B	ΥB	̳B	�JB	�tB	��B	�B	��B	�
B	�hB	��B	��B	��B	��B	�"B	�9B	�'B	}�B	zxB	y>B	xRB	wfB	t�B	q�B	mCB	e�B	b�B	^B	Z�B	YKB	XyB	W�B	V�B	N�B	B�B	?�B	>�B	;�B	:DB	6�B	1�B	+�B	%�B	 �B	xB	~B	�B	mB	�B	TB	�B	DB�"B�B�tB�B�B��B�4B�bB�BܒB�eB�B�YB�OB�B��B��B��B��B��B�VB�jB�B�B�B��B��B��B� B��B�6B�=B�KB�mB��B�aB�{B��B�oB}�Bx�BtnBr�Bo�BnBiBe�BeBd@Bb4B`'B^�B\�BYeBUMBR�BQNBP.BPBPBO(BOBBN�BMjBLJBL~BK�BJ�BIBF�BD�BB'BAB@iB?B=<B9�B8RB6�B5�B5B4B2�B2�B2GB0�B0UB/OB.�B-�B,�B+�B*eB)�B)*B(
B'�B%`B#�B"hB"NB"B!bB �B �B�B�B�B vB�B�B�BVB;B�B!B"hB"�B"B"hB"�B"hB"�B"�B"�B"hB"�B#�B$�B$tB$&B$B$ZB$�B%zB%�B(XB)DB)DB)�B)�B,B-�B/ B.�B0!B2B5%B7�B;�B;�B;B;�B;�B<�B=B>B?cBB�BDMBGEBIBJ=BLdBNpBP}BRTBS&BTBUBUBUBT�BVBUBUgBV�BW�BZ�BZ�BZB[�B\�B^�B`�Bc Be�Bh�Bh�BjBl�Bl�Bm)Bp;Br�Bs3Bu%ButB{JB|�B��B��B��B��B�jB�B��B��B�	B�/B�B�B�B�B��B��B�B�B�B�B�RB��B��B�OB��B��B�ZB�fB��B��B��B��B��B��B��B��B��B��B��B��BǮB��B��B��B�B�B�<B�[B�MB�EB�B��B��B�B�B��B��B��B��B��B��B�B�^B	 4B	MB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	!�B	$&B	(
B	)�B	*0B	,B	-B	./B	./B	1B	3MB	4nB	5�B	9�B	?}B	A�B	A�B	B�B	B�B	B�B	B�B	E�B	F�B	F�B	G�B	I�B	K�B	L�B	M�B	M�B	N�B	N�B	O�B	RB	S@B	W?B	Z7B	\CB	]~B	_pB	`vB	bhB	cnB	cnB	c�B	ffB	gmB	h�B	i�B	iyB	i_B	i�B	iyB	j�B	k�B	l�B	n�B	r�B	t�B	u�B	w�B	|B	� B	�B	�'B	�B	�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<*d�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.07(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201911200032102019112000321020191120003210202211182140562022111821405620221118214056201911210021542019112100215420191121002154  JA  ARFMdecpA19c                                                                20191109213724  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20191109123730  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20191109123732  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20191109123733  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20191109123734  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20191109123734  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20191109123734  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20191109123734  QCF$                G�O�G�O�G�O�            8000JA  ARGQaqcp2.8e                                                                20191109123734  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20191109123734  QCF$                G�O�G�O�G�O�            8000JA      jafc1.0                                                                 20191109123735                      G�O�G�O�G�O�                JA  ARUP                                                                        20191109125551                      G�O�G�O�G�O�                JM  ARGQrqcjv291                                                                20191109153520  QCP$                G�O�G�O�G�O�2DEB7C          JM  ARGQJMQC2.0                                                                 20191109153457  CV  JULD            G�O�G�O�F�T�                JM  ARCAJMQC2.0                                                                 20191119153210  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20191119153210  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20191120152154  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118124056  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111508                      G�O�G�O�G�O�                