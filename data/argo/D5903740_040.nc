CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:08:25Z AOML 3.0 creation; 2016-06-01T00:08:12Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721230825  20160531170812  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               (A   AO  4055_7112_040                   2C  D   APEX                            5374                            041511                          846 @֑:�� 	1   @֑;;���@:BI�^5�cUp��
=1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    (A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dyl�D� D�S3D���D�� D� D�I�D�� D�vfD�	�D�33D��3Dǣ3D�3D�9�Dډ�D���D�fD�VfD��D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�  A  A$  AD  Ad  A�  A�  A�  A�  A�  A�  A�  A�  B  B	  B  B  B!  B)  B1  B9  BA  BI  BQ  BY  Ba  Bi  Bq  By  B�� B�� B�� B�� B�� B�� B��3B��3B�� B�� B�� B�� B�� B�� B�� B�� B�� BĀ BȀ B̀ BЀ BԀ B؀ B܀ B�� B� B� B� B�� B� B�� B�� C @ C@ C@ C@ C@ C
@ C@ C@ C@ C@ C@ C@ C@ C@ C@ C@ C @ C"@ C$@ C&@ C(@ C*@ C,@ C.@ C0@ C2@ C4@ C6@ C8@ C:@ C<@ C>@ C@@ CB@ CD@ CF@ CH@ CJ@ CL@ CN@ CP@ CR@ CT@ CV@ CX@ CZ@ C\@ C^@ C`@ Cb@ Cd@ Cf@ Ch@ Cj@ Cl@ Cn@ Cp@ Cr@ Ct@ Cv@ Cx@ Cz@ C|@ C~@ C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�,�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D  D � D D� D D� D D� D D� D D� D D� D D� D D� D	 D	� D
 D
� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D  D � D! D!� D" D"� D# D#� D$ D$� D% D%� D& D&� D' D'� D( D(� D) D)� D* D*� D+ D+� D, D,� D- D-� D. D.� D/ D/� D0 D0� D1 D1� D2 D2� D3 D3� D4 D4� D5 D5� D6 D6� D7 D7� D8 D8� D9 D9� D: D:� D; D;� D< D<� D= D=� D> D>� D? D?� D@ D@� DA DA� DB DB� DC DC� DD DD� DE DE� DF DF� DG DG� DH DH� DI DI� DJ DJ� DK DK� DL DL� DM DM� DN DN� DO DO� DP DP� DQ DQ� DR DR� DS DS� DT DT� DU DU� DV DV� DW DW� DX DX� DY DY� DZ DZ� D[ D[� D\ D\� D] D]� D^ D^� D_ D_� D` D`� Da Da� Db Db� Dc Dc� Dd Dd� De De� Df Df� Dg Dg� Dh Dh� Di Di� Dj Dj� Dk Dk� Dl Dl� Dm Dm� Dn Dn� Do Do� Dp Dp� Dq Dq� Dr Dr� Ds Ds� Dt Dt�3Dy|�D� D�[3D���D�� D� D�Q�D�� D�~fD��D�;3D��3Dǫ3D�3D�A�Dڑ�D���D�fD�^fD��D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�%A���A��DA��A��A��/A���A��9A���A�$�A���A�/A�A��TA���A��-A��hA��A��A�G�A��A��A���A���A��wA��FA��A���A��A�r�A�`BA�\)A�XA�S�A�9XA��A�JA���A���A��-A���A���A�r�A�O�A�?}A�oA���A��FA���A���A��A�x�A�n�A�dZA�`BA�ZA�M�A�/A�{A���A��mA�ƨA��A��7A�bNA�VA�C�A�5?A�-A�
=A��FA��+A�dZA�A�A�5?A�;dA��wA��A�E�A��PA�-A�~�A���A�VA��#A�E�A��-A�G�A��A�A���A�^5A��DA�JA���A��A�&�A��RA�  A~r�A|r�A{�-Az�AyƨAw;dAt�RAs|�Aqp�Ap(�An�AmK�Ak�
Ag�Ae�-Ad��Ac��Ab�AbffAbJA`��A_&�A]33A[��AY��AVE�AU7LAT1'ASXAQ��AN��AM�TAM7LAL�AK��AJbAH$�AGhsAF��AEXACO�ABȴAB�+AA�FA?�;A>��A>$�A=�A<ZA;+A:M�A9S�A8  A7O�A5�A4�A4-A333A1�A/�-A.��A.z�A-�PA-
=A,�RA,VA+��A+\)A*��A*bNA)S�A(-A&��A%O�A$1'A#A"9XA �/A (�AdZA?}AoA�yA��A=qA�A�A  A�A��A%Ar�A��A|�A��Ar�A�A�A��A"�A�
A�yA��A(�A��A`BA~�AoA9XAO�A
��A
-A	%A�DAZA �A�A�A�mA\)A�Ar�A1'A�PA;dA/A��A�P@��@�x�@��@��D@�%@�^5@�7@�&�@�ƨ@�l�@�;d@�ȴ@��@���@��@�@��@��/@�Z@�F@�n�@�-@�?}@�@���@�@�b@��@��/@�o@��@�t�@���@�@Ӿw@���@�(�@·+@́@̣�@��@ˍP@�;d@���@�n�@ɩ�@��`@ȋD@� �@��@�`B@��/@��@��H@�M�@��#@���@���@��T@�X@���@��@��y@�$�@��h@���@��m@��@�-@�?}@��@��@��y@��@��@�z�@�dZ@��!@��-@�r�@�S�@���@�=q@�7L@��j@��@��m@��@��R@�M�@���@�X@�Z@��@���@��@�@���@�M�@��@��h@�&�@��@�Ĝ@�I�@��
@���@�S�@�
=@�~�@�-@�7L@��9@�z�@� �@��F@���@�;d@�ȴ@��R@�5?@���@��@��D@�9X@�ƨ@�\)@�33@��+@�5?@��h@���@��D@�Q�@�1@���@�dZ@�ȴ@�V@�@��#@��^@��h@�`B@�V@��j@�(�@�1@�  @�  @��F@�C�@�
=@��y@��!@���@�v�@��@�`B@�V@���@��`@���@�Ĝ@��9@�z�@�Q�@�b@���@��P@�33@���@�M�@��@��T@�X@���@��@��@�9X@���@��w@���@�|�@�C�@���@��!@���@��+@�ff@�5?@�{@�J@��@��@��#@�hs@��@���@���@�r�@�1'@��@�"�@��y@���@�v�@�M�@�5?@�$�@��#@���@��7@�p�@�G�@��`@��@�Z@�I�@�A�@��@�@�w@;d@~��@~@}��@}?}@}V@|z�@|9X@{��@{�m@{t�@{"�@{o@{@z~�@y�@yx�@yG�@y�@x��@x��@x�9@xbN@w�;@w\)@v�y@v��@vff@v{@u�-@u/@t��@t�@tz�@t9X@s�
@sdZ@so@q�^@h��@b�@[�
@UO�@Nv�@HbN@A��@<�@4��@0�u@+��@&V@ ��@�@
=@@��@S�@�@�/11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�%A���A��DA��A��A��/A���A��9A���A�$�A���A�/A�A��TA���A��-A��hA��A��A�G�A��A��A���A���A��wA��FA��A���A��A�r�A�`BA�\)A�XA�S�A�9XA��A�JA���A���A��-A���A���A�r�A�O�A�?}A�oA���A��FA���A���A��A�x�A�n�A�dZA�`BA�ZA�M�A�/A�{A���A��mA�ƨA��A��7A�bNA�VA�C�A�5?A�-A�
=A��FA��+A�dZA�A�A�5?A�;dA��wA��A�E�A��PA�-A�~�A���A�VA��#A�E�A��-A�G�A��A�A���A�^5A��DA�JA���A��A�&�A��RA�  A~r�A|r�A{�-Az�AyƨAw;dAt�RAs|�Aqp�Ap(�An�AmK�Ak�
Ag�Ae�-Ad��Ac��Ab�AbffAbJA`��A_&�A]33A[��AY��AVE�AU7LAT1'ASXAQ��AN��AM�TAM7LAL�AK��AJbAH$�AGhsAF��AEXACO�ABȴAB�+AA�FA?�;A>��A>$�A=�A<ZA;+A:M�A9S�A8  A7O�A5�A4�A4-A333A1�A/�-A.��A.z�A-�PA-
=A,�RA,VA+��A+\)A*��A*bNA)S�A(-A&��A%O�A$1'A#A"9XA �/A (�AdZA?}AoA�yA��A=qA�A�A  A�A��A%Ar�A��A|�A��Ar�A�A�A��A"�A�
A�yA��A(�A��A`BA~�AoA9XAO�A
��A
-A	%A�DAZA �A�A�A�mA\)A�Ar�A1'A�PA;dA/A��A�P@��@�x�@��@��D@�%@�^5@�7@�&�@�ƨ@�l�@�;d@�ȴ@��@���@��@�@��@��/@�Z@�F@�n�@�-@�?}@�@���@�@�b@��@��/@�o@��@�t�@���@�@Ӿw@���@�(�@·+@́@̣�@��@ˍP@�;d@���@�n�@ɩ�@��`@ȋD@� �@��@�`B@��/@��@��H@�M�@��#@���@���@��T@�X@���@��@��y@�$�@��h@���@��m@��@�-@�?}@��@��@��y@��@��@�z�@�dZ@��!@��-@�r�@�S�@���@�=q@�7L@��j@��@��m@��@��R@�M�@���@�X@�Z@��@���@��@�@���@�M�@��@��h@�&�@��@�Ĝ@�I�@��
@���@�S�@�
=@�~�@�-@�7L@��9@�z�@� �@��F@���@�;d@�ȴ@��R@�5?@���@��@��D@�9X@�ƨ@�\)@�33@��+@�5?@��h@���@��D@�Q�@�1@���@�dZ@�ȴ@�V@�@��#@��^@��h@�`B@�V@��j@�(�@�1@�  @�  @��F@�C�@�
=@��y@��!@���@�v�@��@�`B@�V@���@��`@���@�Ĝ@��9@�z�@�Q�@�b@���@��P@�33@���@�M�@��@��T@�X@���@��@��@�9X@���@��w@���@�|�@�C�@���@��!@���@��+@�ff@�5?@�{@�J@��@��@��#@�hs@��@���@���@�r�@�1'@��@�"�@��y@���@�v�@�M�@�5?@�$�@��#@���@��7@�p�@�G�@��`@��@�Z@�I�@�A�@��@�@�w@;d@~��@~@}��@}?}@}V@|z�@|9X@{��@{�m@{t�@{"�@{o@{@z~�@y�@yx�@yG�@y�@x��@x��@x�9@xbN@w�;@w\)@v�y@v��@vff@v{@u�-@u/@t��@t�@tz�@t9X@s�
@sdZ@so@q�^@h��@b�@[�
@UO�@Nv�@HbN@A��@<�@4��@0�u@+��@&V@ ��@�@
=@@��@S�@�@�/11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B��B��B��B��B��B��B��BĜB��B�oBn�Bk�BjBl�Bm�Bl�Bl�BffBbNBgmBe`BbNBaHB`BB_;B`BB`BB_;B]/B]/B\)B]/BZBW
BT�BQ�BM�BI�BG�BG�BB�B>wB<jB7LB/B-B+B)�B'�B&�B&�B%�B%�B$�B#�B!�B�B�B�B�BuBbBPBJB
=B1B	7B+B��B��B��B�B�B�B�`B��B�LB�+Be`BO�BPB�NBĜB��Bw�Bk�BJ�B33BJB
�B
ɺB
��B
� B
u�B
dZB
K�B
0!B
 �B
\B
+B	��B	�B	�;B	��B	�}B	�!B	��B	��B	�\B	�B	hsB	[#B	R�B	L�B	G�B	C�B	@�B	8RB	,B	 �B	�B	+B��B�B�mB�NB�#B��B��B��BȴBĜB�dB�?B�-B�!B�B��B��B��B��B��B��B�{B�oB�hB�VB�JB�=B�+B�B�B� B~�B}�B}�B{�B|�B|�B� B�B�B� B� B~�B}�B{�Bx�Bv�Bt�Bt�Bs�Bq�Bn�Bk�BiyBgmBgmBgmBffBe`BcTB`BB_;B_;B`BB_;B^5B\)BZBVBP�BL�BJ�BF�BB�B?}B;dB9XB8RB7LB6FB5?B2-B0!B.B-B+B)�B'�B&�B%�B$�B#�B!�B �B�B�B�B�B�B�B�B�BuBhB\BPBDB
=B
=B	7B1B1B1B1B%B+B%B%B%BBBBBBBBBBBBBBBB+B1B	7BDBJBVB\BbBoBuBuBuB{B{B�B�B�B�B�B�B�B�B�B�B�B �B"�B$�B$�B%�B'�B(�B)�B+B-B.B0!B2-B49B5?B6FB8RB:^B=qB>wB@�BB�BD�BH�BJ�BL�BM�BP�BQ�BS�BT�BXB[#B[#B`BBdZBhsBiyBjBjBjBjBl�Bo�Bq�Bs�Bs�Bt�Bv�Bx�Bx�Bz�B{�B� B�B�7B�VB�bB�uB��B��B��B��B��B��B��B��B��B��B��B�B�B�B�3B�^B�}B��BÖBŢBƨBɺB��B��B��B��B��B�B�
B�B�#B�;B�NB�TB�NB�ZB�sB�yB�yB�B�B�B�B��B��B��B��B��B��B��B��B��B��B	B	B	B	+B	DB	JB	VB	oB	�B	�B	�B	�B	�B	�B	!�B	"�B	#�B	(�B	(�B	)�B	+B	,B	.B	0!B	0!B	2-B	33B	33B	7LB	9XB	:^B	>wB	?}B	A�B	F�B	L�B	N�B	Q�B	R�B	S�B	T�B	VB	YB	]/B	^5B	_;B	`BB	e`B	jB	l�B	l�B	l�B	n�B	p�B	q�B	s�B	v�B	w�B	w�B	x�B	z�B	}�B	~�B	� B	� B	�B	�%B	�+B	�7B	�DB	�PB	�VB	�bB	�hB	�oB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�FB	��B	�sB	��B
%B
{B
�B
)�B
2-B
<jB
A�B
G�B
N�B
T�B
[#B
aHB
ffB
l�B
q�B
u�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B��B��B��B��B��B��BĉB��B�XBn�BkpBjiBlwBm{BlxBlxBfOBb;BgZBeNBb<Ba3B`/B_)B`1B`-B_$B]B]B\B]BZ	BV�BT�BQ�BM�BI�BG�BG�BBzB>bB<WB76B/B,�B*�B)�B'�B&�B&�B%�B%�B$�B#�B!�B�B�B�BjB_BLB;B5B
(BB	!BB��B��B��B�B��B�B�LB��B�6B�BeGBO�B4B�5BĆB��Bw�BkjBJ�B3B7B
�sB
ɤB
��B
�B
u�B
dFB
K�B
0B
 �B
LB
B	��B	�B	�+B	˵B	�kB	�B	��B	��B	�NB	��B	heB	[B	R�B	L�B	G�B	C�B	@vB	8EB	+�B	 �B	wB	B��B�B�cB�FB�B��B��B˿BȬBĖB�]B�9B�%B�B�B��B��B��B��B��B��B�uB�kB�bB�NB�CB�6B�'B�B�B�B~�B}�B}�B{�B|�B|�B�B�B�B�B�B~�B}�B{�Bx�Bv�Bt�Bt�Bs�Bq�Bn�BkBisBgiBgiBggBfaBe]BcNB`=B_8B_7B`>B_2B^0B\#BZBU�BP�BL�BJ�BF�BB�B?xB;aB9;B8MB7HB6?B5;B2)B0 B.B-B*�B)�B'�B&�B%�B$�B#�B!�B �B�B�B�B�B�B�B{BoBWBJB<B2B&B
B
 B	BBBBB BBB#BB�BB B BB B B�B�B�B�B�BBBB&BB	B$B(BOB:B]BjBUBnBnB[BvB{BfB�BiB�B�B�BB�B�B�B �B"�B$�B$�B%�B'�B(�B)�B*�B-B.B0B2&B4/B58B6>B8IB:UB=jB>pB@yBB�BD�BH�BJ�BL�BM�BP�BQ�BS�BT�BXB[B[B`8BdMBhjBipBjuBjtBjuBjwBl~Bo�Bq�Bs�Bs�Bt�Bv�Bx�Bx�Bz�B{�B�B� B�+B�IB�QB�eB�{B�yB��B��B��B��B��B��B��B��B��B��B��B�B�!B�OB�mB�wBÇBŏBƕBɪB��B��B��B��B��B��B��B�B�B�(B�?B�DB�<B�JB�eB�fB�hB�vB�qB�B�B��B��B��B��B��B��B��B��B��B��B	�B	�B	B	B	2B	7B	EB	[B	rB	xB	�B	�B	�B	�B	!�B	"�B	#�B	(�B	(�B	)�B	*�B	+�B	-�B	0B	0B	2B	3B	3B	76B	9BB	:JB	>aB	?iB	AsB	F�B	L�B	N�B	Q�B	R�B	S�B	T�B	U�B	YB	]B	^B	_&B	`+B	eJB	jhB	luB	ltB	lrB	n�B	p�B	q�B	s�B	v�B	w�B	w�B	x�B	z�B	}�B	~�B	�B	�B	��B	�B	�B	� B	�,B	�9B	�>B	�KB	�RB	�WB	�WB	�]B	�dB	�oB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�-B	��B	�[B	��B
B
aB
�B
)�B
2B
<NB
AmB
G�B
N�B
T�B
[B
a*B
fJB
loB
q�B
u�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.25 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708122016053117081220160531170812  AO  ARCAADJP                                                                    20140721230825    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230825  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230825  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170812  IP                  G�O�G�O�G�O�                