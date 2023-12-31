CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:05Z UW 3.1 conversion   
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  T8   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ]�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  eP   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  g8   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  n�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  vh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  xP   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �h   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �$   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �(   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �8   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �<   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �@   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               2A   AO  20111130140218  20190522121826  1727_5046_050                   2C  D   APEX                            2143                            040306                          846 @�VB+���1   @�VB�b�@7�O�;d�c�V�u1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  @���A   A@  A`  A�  A�  A�  A�  A���A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D	  D	� D
  D
� DfD�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D �fD!  D!� D"  D"�fD#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Diy�Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dm��Dn� Do  Do� Dp  Dp�fDq  Dq� Dr  Dry�Ds  Ds� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�ff@�  @���A   A@  A`  A�  A�  A�  A�  A���A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D	  D	� D
  D
� DfD�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D �fD!  D!� D"  D"�fD#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Diy�Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dm��Dn� Do  Do� Dp  Dp�fDq  Dq� Dr  Dry�Ds  Ds� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�dZA�bNA�bNA�\)A�XA�XA�ZA�ZA�\)A�\)A�VA�O�A�O�A�M�A�I�A�E�A�?}A�1'A�(�A�
=A�1A�%A��yA��TA���A���A¾wA²-A¬A§�A£�A®Aº^A¶FA©�A¶FA�ƨA©�A�|�A�n�A�jA�p�AA�A¶FA�A��/AA�O�A���A�
=A���A�S�A�E�A��A�dZA�7LA�Q�A�+A��/A��A�bA��A���A���A���A��RA���A��9A��;A��yA�v�A�A�t�A���A��HA��+A���A�(�A�A���A�33A�ƨA�\)A��A�l�A���A��;A���A���A���A��A�O�A��A�r�A� �A���A�1'A�7LA���A�ffA�t�A�ĜA�Q�A���A�(�A��PA��^A��PA��A��A�XA��TA��DA�VA�1A�bNA��TA�v�A��
A�;dA��A�A~ZA}�7A|Az��Aw�PAuƨAt��At1Ar��Ar$�Aq�Aql�Ao�wAnbNAl��AkXAj��AjVAgƨAgC�Afz�Ad�/Ac7LAb  AaG�A_��A]�-A[p�AX�AVA�AUO�ASx�AR$�AP�AO��AOoAM��AL��AL��AL-AK
=AJffAI��AH��AHv�AG��AFz�AEC�AD��AC�hAB9XA@��A??}A>I�A>��A>n�A<�A;oA9ƨA8ȴA7��A6VA5�A3�-A1��A05?A/O�A-�#A-��A-G�A+�A+�A*�A*�RA*A�A)+A'�wA&�A%O�A$jA#�A#"�A!��A �HA �A7LAVA��A��A/A�/A�`A�yAJAVA=qA�AE�A�-A�#AAA�A��A~�A�A�A��Az�AAt�A�!A�mA`BA�uA%A	�A	`BAQ�A&�A�A%A�AM�A��A�A��A^5A��@��w@�5?@�  @�V@��@��
@�E�@�/@�Q�@��@�/@�
=@���@�P@柾@���@��@�K�@�h@��@߅@ޟ�@݁@ܛ�@�  @�$�@��;@�|�@��y@��T@ӍP@щ7@�1'@Ͼw@�K�@Η�@�`B@�S�@Ɂ@��m@��#@ēu@��@�M�@���@�V@�`B@�Z@�  @� �@��u@���@��@��@�I�@�t�@��y@��R@�@��@���@�z�@���@�$�@�$�@���@��`@�z�@�A�@��m@��@�v�@��@���@�Ĝ@��@��+@���@���@�33@���@���@��!@��#@��@�(�@��@��;@���@�-@��@�`B@��`@�9X@�ƨ@�t�@��@�n�@�$�@�X@�V@���@��@�S�@�"�@��@��!@�E�@��^@�p�@�7L@�&�@��`@��@��u@�r�@��@��m@��w@�|�@�33@��@��R@��+@�M�@�E�@�n�@�ff@�V@�J@��#@��h@�p�@�p�@� �@�1@��@���@�  @��@��@��@��
@�l�@��H@���@���@�~�@�$�@�@��^@�X@��@��`@���@��9@��D@�9X@�t�@�33@��@�V@��@�@�X@�G�@��@��F@�l�@�l�@�\)@�+@��H@��@���@��-@�X@�7L@�V@���@���@��@�bN@��9@��@��h@��/@�?}@�G�@��/@�9X@��@���@�{@��h@��7@�x�@�`B@��@��j@��9@��u@�I�@�(�@��;@���@�l�@�o@���@���@���@��+@�ff@�E�@�J@��T@��h@�`B@�G�@�7L@�V@���@��j@���@��@� �@�  @~ff@~��@}@}@}��@}O�@}O�@|j111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�dZA�bNA�bNA�\)A�XA�XA�ZA�ZA�\)A�\)A�VA�O�A�O�A�M�A�I�A�E�A�?}A�1'A�(�A�
=A�1A�%A��yA��TA���A���A¾wA²-A¬A§�A£�A®Aº^A¶FA©�A¶FA�ƨA©�A�|�A�n�A�jA�p�AA�A¶FA�A��/AA�O�A���A�
=A���A�S�A�E�A��A�dZA�7LA�Q�A�+A��/A��A�bA��A���A���A���A��RA���A��9A��;A��yA�v�A�A�t�A���A��HA��+A���A�(�A�A���A�33A�ƨA�\)A��A�l�A���A��;A���A���A���A��A�O�A��A�r�A� �A���A�1'A�7LA���A�ffA�t�A�ĜA�Q�A���A�(�A��PA��^A��PA��A��A�XA��TA��DA�VA�1A�bNA��TA�v�A��
A�;dA��A�A~ZA}�7A|Az��Aw�PAuƨAt��At1Ar��Ar$�Aq�Aql�Ao�wAnbNAl��AkXAj��AjVAgƨAgC�Afz�Ad�/Ac7LAb  AaG�A_��A]�-A[p�AX�AVA�AUO�ASx�AR$�AP�AO��AOoAM��AL��AL��AL-AK
=AJffAI��AH��AHv�AG��AFz�AEC�AD��AC�hAB9XA@��A??}A>I�A>��A>n�A<�A;oA9ƨA8ȴA7��A6VA5�A3�-A1��A05?A/O�A-�#A-��A-G�A+�A+�A*�A*�RA*A�A)+A'�wA&�A%O�A$jA#�A#"�A!��A �HA �A7LAVA��A��A/A�/A�`A�yAJAVA=qA�AE�A�-A�#AAA�A��A~�A�A�A��Az�AAt�A�!A�mA`BA�uA%A	�A	`BAQ�A&�A�A%A�AM�A��A�A��A^5A��@��w@�5?@�  @�V@��@��
@�E�@�/@�Q�@��@�/@�
=@���@�P@柾@���@��@�K�@�h@��@߅@ޟ�@݁@ܛ�@�  @�$�@��;@�|�@��y@��T@ӍP@щ7@�1'@Ͼw@�K�@Η�@�`B@�S�@Ɂ@��m@��#@ēu@��@�M�@���@�V@�`B@�Z@�  @� �@��u@���@��@��@�I�@�t�@��y@��R@�@��@���@�z�@���@�$�@�$�@���@��`@�z�@�A�@��m@��@�v�@��@���@�Ĝ@��@��+@���@���@�33@���@���@��!@��#@��@�(�@��@��;@���@�-@��@�`B@��`@�9X@�ƨ@�t�@��@�n�@�$�@�X@�V@���@��@�S�@�"�@��@��!@�E�@��^@�p�@�7L@�&�@��`@��@��u@�r�@��@��m@��w@�|�@�33@��@��R@��+@�M�@�E�@�n�@�ff@�V@�J@��#@��h@�p�@�p�@� �@�1@��@���@�  @��@��@��@��
@�l�@��H@���@���@�~�@�$�@�@��^@�X@��@��`@���@��9@��D@�9X@�t�@�33@��@�V@��@�@�X@�G�@��@��F@�l�@�l�@�\)@�+@��H@��@���@��-@�X@�7L@�V@���@���@��@�bN@��9@��@��h@��/@�?}@�G�@��/@�9X@��@���@�{@��h@��7@�x�@�`B@��@��j@��9@��u@�I�@�(�@��;@���@�l�@�o@���@���@���@��+@�ff@�E�@�J@��T@��h@�`B@�G�@�7L@�V@���@��j@���@��@� �@�  @~ff@~��@}@}@}��@}O�@}O�@|j111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBD�BE�BE�BF�BE�BE�BD�BF�BF�BF�BG�BG�BG�BG�BF�BF�BF�BF�BG�BG�BG�BG�BG�BG�BI�BK�BP�BZBe`B}�B��B��B�3B�LB�RB�dB��B�jB�LB�^B�qB��BɺB��B�#B�5B�sB�B�B�B�TB�/B�B��BɺB�!B��B�bB��B��B��B�B�B��B��B��B��B��B�\B�By�Bu�Bq�Bm�BffB^5BZBYBM�B>wB+B"�B�B�BoB1B��B�fB�wB��B�+Bx�BjBZBQ�BM�BI�BB�B5?B,B�B{B
=BB
��B
��B
�B
�fB
�
B
��B
�FB
�B
��B
��B
��B
��B
��B
�bB
�=B
�B
w�B
n�B
gmB
^5B
XB
N�B
C�B
8RB
,B
%�B
�B
�B
�B
oB
PB
B	��B	�B	�B	�sB	�TB	�
B	��B	��B	�}B	�9B	�B	�B	��B	��B	�JB	|�B	w�B	u�B	k�B	dZB	aHB	_;B	[#B	YB	XB	VB	T�B	VB	R�B	N�B	K�B	J�B	C�B	;dB	49B	/B	'�B	�B	oB	%B	B	PB	\B	B�B�mB�BB�B�B��BɺBÖB�wB�XB�LB�FB�?B�FB�dB�LB�XB�LB�'B�B��B��B��B��B��B��B��B��B��B�{B�{B�uB�uB�uB�uB�oB�hB�PB�=B�DB�7B�%B�B�B~�B{�By�Bw�Bv�Bu�Bs�Bq�Bo�Bl�BjBhsBdZBaHB_;B[#BW
BT�BR�BQ�BP�BO�BM�BK�BJ�BH�BD�BA�B?}B=qB:^B7LB8RB7LB6FB5?B33B1'B0!B0!B/B/B.B-B-B-B-B-B-B.B.B-B-B1'B1'B0!B0!B2-B33B49B5?B6FB:^B;dB8RB6FB49B33B5?B5?B6FB6FB<jBH�BI�BM�BR�BYB]/B[#B]/B`BB_;B^5BaHB`BB^5B_;B_;BcTBcTBcTBcTBgmBiyBjBk�Bl�Bn�Bn�Bl�BjBjBjBjBiyBm�Bo�Bo�Bo�Br�By�B� B�B�B�7B�PB�VB�bB�{B��B��B��B��B��B��B��B�B�B�LB�jB�wB��BÖBɺB��B��B�
B�B�)B�BB�HB�HB�`B�yB�B�B�B��B��B��B	B	%B	1B		7B	
=B	VB	bB	�B	�B	�B	#�B	(�B	.B	1'B	2-B	49B	49B	49B	6FB	:^B	A�B	E�B	E�B	H�B	O�B	Q�B	S�B	VB	XB	ZB	\)B	^5B	`BB	bNB	iyB	k�B	l�B	m�B	n�B	n�B	m�B	m�B	k�B	m�B	p�B	q�B	q�B	q�B	q�B	t�B	v�B	x�B	y�B	z�B	z�B	|�B	~�B	� B	�B	�1B	�DB	�PB	�VB	�uB	��B	�{B	�oB	�\B	�\B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�'B	�-B	�?B	�FB	�LB	�RB	�XB	�dB	�qB	�wB	��B	��B	B	B	ĜB	ŢB	ɺ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BD�BE�BE�BF�BE�BE�BD�BF�BF�BF�BG�BG�BG�BG�BF�BF�BF�BF�BG�BG�BG�BG�BG�BG�BI�BK�BP�BZBe`B}�B��B��B�3B�LB�RB�dB��B�qB�LB�^B�qB��BɺB��B�#B�5B�B�B�B�B�mB�5B�)B�B��B�LB��B�{B��B��B�B�9B�B�B�B��B��B��B��B�+B{�Bw�Bs�Bp�BjB`BB\)B]/BR�BG�B/B$�B�B�B�BbB��B�BǮB��B�\B}�Bq�B^5BS�BO�BL�BG�B7LB1'B#�B�BJB1B  B
��B
�B
�B
�B
��B
�^B
�B
��B
��B
��B
��B
��B
�oB
�PB
�B
y�B
p�B
iyB
aHB
\)B
S�B
L�B
>wB
/B
(�B
#�B
�B
�B
{B
oB
	7B
B	��B	�B	�B	�B	�B	�B	��B	ĜB	�RB	�'B	�3B	�B	��B	��B	�B	z�B	z�B	n�B	hsB	cTB	`BB	^5B	\)B	YB	W
B	XB	XB	T�B	P�B	M�B	M�B	F�B	?}B	6FB	2-B	,B	$�B	�B		7B	  B	\B	�B		7B��B�B�ZB�5B�B�B��BȴB��B�wB�RB�RB�^B�RB�qB�RB�dB�dB�FB�B�B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�uB��B�{B�\B�PB�PB�DB�DB�B�B�B|�B{�Bx�Bx�Bw�Bt�Bs�Bq�Bn�Bl�Bk�BiyBdZBaHB_;B[#BYBVBR�BR�BQ�BP�BM�BK�BK�BJ�BD�BC�BA�BA�B=qB;dB9XB8RB8RB6FB5?B49B33B1'B1'B0!B0!B0!B/B-B/B/B0!B/B0!B-B1'B2-B1'B33B5?B5?B5?B6FB7LB<jB>wB;dB9XB7LB5?B6FB8RB8RB;dB>wBJ�BJ�BM�BQ�BYB]/B`BB^5BbNB`BB_;BcTBbNB_;B`BBbNBdZBcTBdZBdZBhsBjBjBl�Bn�Bo�Bn�Bn�Bm�Bk�Bl�Bl�Bl�Bn�Bo�Bo�Bq�Bt�Bz�B� B�B�B�=B�PB�\B�hB��B��B��B��B��B��B��B��B�B�!B�RB�jB�wB��BĜB��B��B��B�
B�B�/B�BB�HB�NB�`B�yB�B�B�B��B��B��B	B	%B	1B		7B	DB	VB	hB	�B	�B	�B	#�B	(�B	.B	1'B	2-B	49B	49B	5?B	7LB	;dB	B�B	E�B	E�B	I�B	O�B	R�B	T�B	VB	XB	ZB	\)B	^5B	aHB	cTB	jB	l�B	m�B	n�B	n�B	o�B	m�B	m�B	m�B	m�B	p�B	q�B	q�B	r�B	r�B	t�B	v�B	y�B	y�B	z�B	z�B	|�B	~�B	� B	�B	�+B	�=B	�VB	�PB	�uB	��B	��B	�{B	�bB	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�'B	�-B	�?B	�LB	�LB	�RB	�^B	�dB	�wB	�wB	��B	��B	B	B	ĜB	ƨB	ɺ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446512012010314465120120103144651  AO  ARGQ                                                                        20111130140218  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130140218  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144651  IP                  G�O�G�O�G�O�                