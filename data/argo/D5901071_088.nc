CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:15Z UW 3.1 conversion   
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
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               XA   AO  20111130141145  20190522121826  1727_5046_088                   2C  D   APEX                            2143                            040306                          846 @ԇf]K��1   @ԇg��@7�+I��d��`A�1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B��B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBx  B�  B�  B���B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C�C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CA�fCD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\�C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cm�fCp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG�fDH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  Dey�Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dky�Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dpy�Dp��Dq� DrfDr� Dr��Ds� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B��B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBx  B�  B�  B���B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C�C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CA�fCD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\�C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cm�fCp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG�fDH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  Dey�Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dky�Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dpy�Dp��Dq� DrfDr� Dr��Ds� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AϓuAϕ�Aϗ�Aϗ�Aϗ�Aϙ�Aϙ�Aϛ�Aϙ�Aϙ�Aϛ�Aϗ�Aϗ�Aϙ�Aϝ�Aϟ�Aϡ�Aϡ�Aϟ�Aϡ�Aϡ�Aϡ�Aϣ�Aϡ�Aϙ�AϓuAχ+A�r�A�/AΥ�A͝�A�-Aʺ^AȁA�l�AƝ�A���A�-A�A�Q�A�+A���A�n�A�bA��!A��A�z�A���A�O�A�A�ƨA��A�ƨA���A�VA��`A��A�t�A���A��7A�n�A�
=A�n�A�5?A�ȴA�$�A�(�A���A���A���A�$�A�;dA��A��TA���A�XA��A��TA��A�-A��yA��A��A�A�A�XA�\)A���A��#A���A�VA�A�ƨA��A�l�A�VA�VA��RA��\A��A��wA��A�VA��A���A�x�A�{A��A��#A�VA��A��\A���A�`BA���A��9A�bA�dZA��A��A�1A��+A��-A���A��A��HA�A}�mA{�AydZAx�jAw�Au�At1As\)AqƨAq�Ao"�An��Aml�Ak|�Ai�wAi7LAhAf��Ae�7AcVA`�yA^��A]��A]G�A[�AZ~�AYK�AV�HAU��AU7LAT��AT�AS�
AS�AR��AR^5AQ��AP$�AOS�ANM�AMhsAM�AL��AKG�AJQ�AI�AH��AH=qAG�PAF�9AFbNAD��AC��ABI�AA��AAS�A@�\A@  A?�FA>�A=?}A;33A:A�A9�hA8ȴA8VA7�A5��A4�uA3?}A2jA2�A1�A0�jA01A/`BA.�RA-\)A,I�A+�A*A�A)"�A(-A'XA&ȴA%�PA$�+A#"�A"�A!��A �`A��A�uA�;A
=A��A��A��A�#A��AƨA9XAXA�FA�jAZA�AAI�A�;AK�A��A�\A(�A�
A
E�A	�A	�A��AA�A�-A�AE�A�7Al�AS�A�`AI�A�A�wA�-A��A�A ��A $�@��;@�dZ@�O�@��H@���@�l�@�G�@���@�dZ@�$�@�7@�+@��;@�t�@�|�@�ȴ@�7L@�Ĝ@�?}@�7L@�bN@���@���@�V@ݲ-@�`B@ܬ@ۍP@��@�V@���@�9X@׾w@�^5@�J@�G�@��@�|�@�ƨ@Ӿw@�@́@�A�@�"�@�^5@���@�7L@��@�o@���@���@Ĵ9@�l�@�@�ff@�=q@�{@��@��@�?}@��9@��@�ff@�@�hs@�&�@��@�j@�S�@��\@���@�Z@�(�@�ƨ@�K�@���@�n�@��^@�bN@���@�+@�(�@��@��
@�bN@�@��y@�=q@��m@�@���@��@�+@�%@��-@�/@�V@��`@��m@���@���@���@���@���@��@�"�@�ff@��#@�%@��@��@��R@�v�@�5?@���@��/@�r�@�ƨ@��w@�\)@�C�@��@���@���@���@���@�`B@�?}@��/@��9@��@��/@��@�7L@�/@�%@���@��j@�A�@��@� �@�1@��F@��@�n�@���@���@���@���@�X@�X@�O�@���@���@���@�r�@�b@��@�ƨ@�;d@��H@��@�ȴ@�~�@�$�@���@�x�@��^@�Ĝ@�@�{@��^@���@���@�Z@��;@��;@���@���@� �@���@�O�@��@�=q@�n�@��@�/@���@�Ĝ@�7L@���@���@��!@�l�@�|�@�dZ@�33@�@��@���@�n�@��#@��^@��h@��@��@��/@���@�Ĝ@��j@��9@���@��@�1@��@�dZ@�33@�@���@��\@�n�@��@���@���@��h111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AϓuAϕ�Aϗ�Aϗ�Aϗ�Aϙ�Aϙ�Aϛ�Aϙ�Aϙ�Aϛ�Aϗ�Aϗ�Aϙ�Aϝ�Aϟ�Aϡ�Aϡ�Aϟ�Aϡ�Aϡ�Aϡ�Aϣ�Aϡ�Aϙ�AϓuAχ+A�r�A�/AΥ�A͝�A�-Aʺ^AȁA�l�AƝ�A���A�-A�A�Q�A�+A���A�n�A�bA��!A��A�z�A���A�O�A�A�ƨA��A�ƨA���A�VA��`A��A�t�A���A��7A�n�A�
=A�n�A�5?A�ȴA�$�A�(�A���A���A���A�$�A�;dA��A��TA���A�XA��A��TA��A�-A��yA��A��A�A�A�XA�\)A���A��#A���A�VA�A�ƨA��A�l�A�VA�VA��RA��\A��A��wA��A�VA��A���A�x�A�{A��A��#A�VA��A��\A���A�`BA���A��9A�bA�dZA��A��A�1A��+A��-A���A��A��HA�A}�mA{�AydZAx�jAw�Au�At1As\)AqƨAq�Ao"�An��Aml�Ak|�Ai�wAi7LAhAf��Ae�7AcVA`�yA^��A]��A]G�A[�AZ~�AYK�AV�HAU��AU7LAT��AT�AS�
AS�AR��AR^5AQ��AP$�AOS�ANM�AMhsAM�AL��AKG�AJQ�AI�AH��AH=qAG�PAF�9AFbNAD��AC��ABI�AA��AAS�A@�\A@  A?�FA>�A=?}A;33A:A�A9�hA8ȴA8VA7�A5��A4�uA3?}A2jA2�A1�A0�jA01A/`BA.�RA-\)A,I�A+�A*A�A)"�A(-A'XA&ȴA%�PA$�+A#"�A"�A!��A �`A��A�uA�;A
=A��A��A��A�#A��AƨA9XAXA�FA�jAZA�AAI�A�;AK�A��A�\A(�A�
A
E�A	�A	�A��AA�A�-A�AE�A�7Al�AS�A�`AI�A�A�wA�-A��A�A ��A $�@��;@�dZ@�O�@��H@���@�l�@�G�@���@�dZ@�$�@�7@�+@��;@�t�@�|�@�ȴ@�7L@�Ĝ@�?}@�7L@�bN@���@���@�V@ݲ-@�`B@ܬ@ۍP@��@�V@���@�9X@׾w@�^5@�J@�G�@��@�|�@�ƨ@Ӿw@�@́@�A�@�"�@�^5@���@�7L@��@�o@���@���@Ĵ9@�l�@�@�ff@�=q@�{@��@��@�?}@��9@��@�ff@�@�hs@�&�@��@�j@�S�@��\@���@�Z@�(�@�ƨ@�K�@���@�n�@��^@�bN@���@�+@�(�@��@��
@�bN@�@��y@�=q@��m@�@���@��@�+@�%@��-@�/@�V@��`@��m@���@���@���@���@���@��@�"�@�ff@��#@�%@��@��@��R@�v�@�5?@���@��/@�r�@�ƨ@��w@�\)@�C�@��@���@���@���@���@�`B@�?}@��/@��9@��@��/@��@�7L@�/@�%@���@��j@�A�@��@� �@�1@��F@��@�n�@���@���@���@���@�X@�X@�O�@���@���@���@�r�@�b@��@�ƨ@�;d@��H@��@�ȴ@�~�@�$�@���@�x�@��^@�Ĝ@�@�{@��^@���@���@�Z@��;@��;@���@���@� �@���@�O�@��@�=q@�n�@��@�/@���@�Ĝ@�7L@���@���@��!@�l�@�|�@�dZ@�33@�@��@���@�n�@��#@��^@��h@��@��@��/@���@�Ĝ@��j@��9@���@��@�1@��@�dZ@�33@�@���@��\@�n�@��@���@���@��h111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBF�BF�BF�BF�BF�BE�BF�BF�BE�BF�BF�BE�BE�BE�BE�BF�BF�BF�BE�BE�BE�BE�BE�BE�BE�BD�BC�BB�B>wB8RB,B�B�B%�B.B33BD�B]/BdZBe`Be`Bm�B�1B�VB�{B��B��B��B�B�B�-B�B�B�B�LB�dB�RB�RB�wB��B��B��BɺB��B��B��B��B��B��B��B�
B�
B�B��B��B��B��B��B��B��B��B��B�3B�B��B��B��B�VB�B{�Bn�BaHBO�BB�B;dB.B�BhBDBB��B�`B�
BB�3B�B��B��B�{B�JBw�BgmBJ�B2-B�BB
�sB
ƨB
�!B
��B
��B
�bB
�B
|�B
w�B
k�B
_;B
N�B
F�B
A�B
8RB
1'B
+B
%�B
�B
�B
oB
\B
1B	��B	��B	�B	�B	�HB	�B	��B	ȴB	��B	�XB	�FB	�B	��B	��B	�oB	�JB	�1B	�B	�B	�B	~�B	{�B	x�B	s�B	k�B	e`B	aHB	]/B	aHB	bNB	^5B	[#B	YB	XB	R�B	M�B	J�B	G�B	?}B	9XB	33B	1'B	.B	+B	(�B	%�B	�B	�B	VB	
=B	%B	B	  B��B�B�B�B�mB�`B�TB�;B�)B�B��B��BɺBŢB��B�jB�XB�FB�3B�B�B��B��B��B��B��B�uB�hB�PB�=B�+B�B~�Bz�Bv�Bs�Bm�BiyBgmBgmBffBe`Be`BffBffBffBdZBcTBaHB_;B]/BZBXBVBR�BP�BN�BO�BO�BN�BM�BL�BJ�BI�BG�BF�BC�BA�B@�B@�B>wB=qB;dB7LB6FB6FB33B6FB:^B9XB:^B=qB?}B?}B?}BA�BB�BE�BI�BL�BM�BM�BO�BQ�BQ�BR�BS�BT�BR�BT�BYB]/B[#B^5B_;B_;BbNBffBdZB[#BR�BP�BQ�BR�BW
BW
BVBW
BXB[#B]/B`BBbNBe`Bk�Bq�Bs�Bw�Bz�B�B�B�+B�DB�=B�=B�DB�JB�\B�oB�oB�oB��B��B��B��B��B��B��B��B��B��B��B��B�B�XB��B�qB�LB��B��B��B��B�3B��B��B��B��B��BǮBǮBƨBB�wB�XB�RB�XB�dB�jB�}BÖBÖBB��BÖBǮB��B��B��B��B�
B�B�B�B�B�#B�5B�fB�yB�B�B�B��B	B	B	B	B	B	%B		7B	
=B	
=B	
=B	DB	PB	PB	VB	bB	�B	�B	�B	�B	 �B	 �B	"�B	$�B	%�B	$�B	%�B	,B	.B	0!B	33B	49B	5?B	7LB	;dB	A�B	A�B	B�B	D�B	F�B	H�B	K�B	L�B	N�B	S�B	YB	\)B	aHB	gmB	jB	l�B	q�B	w�B	y�B	x�B	w�B	w�B	z�B	w�B	{�B	�1B	�VB	�\B	�\B	�\B	�\B	�\B	�bB	�hB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�-B	�9B	�3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BF�BF�BF�BF�BF�BE�BF�BF�BE�BF�BF�BE�BE�BE�BE�BF�BF�BF�BE�BE�BE�BE�BE�BE�BE�BD�BC�BC�B?}B:^B0!B"�B!�B)�B1'B:^BL�B`BBe`BffBffBq�B�7B�\B��B��B��B��B�B�9B�?B�B�B�!B�XB�qB�qB�jB�}B��B��B��B��B��B��B��B��B��B��B�
B�)B�B�
B�
B��B��B��B��B��B��B��B�
B�XB�LB��B��B��B�oB�7B�Bt�BiyBR�BD�B@�B5?B$�B{BPBDB��B�yB�BB��B�?B�!B��B��B��B�oB{�Bp�BP�B8RB&�BJB
��B
��B
�?B
��B
��B
��B
�+B
~�B
}�B
p�B
gmB
S�B
H�B
F�B
=qB
6FB
-B
+B
!�B
 �B
{B
oB
JB
B	��B	�B	�B	�TB	�;B	�
B	��B	B	�dB	�XB	�'B	��B	��B	�{B	�VB	�7B	�+B	�B	�B	� B	}�B	z�B	w�B	m�B	hsB	dZB	^5B	cTB	ffB	aHB	]/B	[#B	ZB	T�B	P�B	K�B	L�B	B�B	=qB	5?B	33B	0!B	-B	)�B	)�B	#�B	�B	hB	JB		7B	B	B��B��B�B�B�sB�mB�fB�NB�5B�#B�B��B��BȴBŢB�}B�dB�RB�FB�'B�B��B��B��B��B��B��B�uB�hB�PB�=B�1B�B}�Bz�Bv�Br�Bl�BhsBhsBiyBgmBffBhsBhsBgmBe`BdZBffB`BB_;B\)BZBXBT�BS�BP�BP�BP�BP�BO�BM�BK�BJ�BJ�BJ�BG�BC�BA�BA�BA�BA�B>wB=qB9XB9XB7LB8RB;dB=qB>wB>wB?}B@�BA�BB�BB�BE�BJ�BM�BM�BP�BP�BR�BR�BT�BT�BVBS�BXBZB_;B\)B`BBaHB`BBbNBgmBhsB[#BT�BR�BR�BS�BXBYBXBYBZB\)B_;BaHBbNBe`Bl�Bs�Bs�Bw�B{�B�+B�%B�1B�JB�DB�=B�JB�VB�hB�{B�{B�uB��B��B��B��B��B��B��B��B��B��B��B��B�B�RB��B��B�}B��B��B��B��B�B��B��B��B��B��BǮBǮBȴBĜBŢB�dB�^B�^B�qB�wB��BĜBĜBÖBBŢBȴB��B��B��B��B�B�)B�B�B�B�)B�5B�mB�yB�B�B�B��B	B	B	B	B	%B	%B		7B	
=B	DB	DB	JB	VB	VB	VB	bB	{B	�B	�B	�B	!�B	 �B	"�B	%�B	%�B	$�B	&�B	-B	.B	0!B	49B	5?B	6FB	7LB	;dB	C�B	D�B	D�B	E�B	H�B	H�B	L�B	M�B	N�B	T�B	YB	[#B	`BB	ffB	jB	k�B	q�B	x�B	z�B	y�B	w�B	w�B	� B	y�B	y�B	�+B	�VB	�\B	�\B	�\B	�\B	�\B	�hB	�oB	�oB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�-B	�9B	�3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447042012010314470420120103144704  AO  ARGQ                                                                        20111130141145  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130141145  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144704  IP                  G�O�G�O�G�O�                