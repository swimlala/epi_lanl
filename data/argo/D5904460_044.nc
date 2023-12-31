CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-04-15T19:37:21Z AOML 3.0 creation; 2016-08-07T21:17:35Z UW 3.1 conversion     
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
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Cx   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  KX   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  MP   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U0   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  x�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  z�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �x   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �p   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �P   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �$   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �(   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20150415193721  20160807141736  5904460 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               ,A   AO  5285_8895_044                   2C  D   APEX                            6487                            072314                          846 @�IR`U 1   @�IS�_�@.=�E���c�\(�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    ,A   B   B   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BW��B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B���B���B�33B���B�  B�  B�  B�  B�  B�  B�  B�33B���B���B���B�  B�  B�  B�  B�  B�  B���B�  B�33B�  C   C  C  C  C  C
  C  C  C  C  C  C  C�C�C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch�Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.y�D/  D/� D0fD0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy&fD��D�I�D�|�D���D��D�0 D��fD��fD� D�FfD�s3Dǣ3D�C3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@Y��@�  @�  A  A(  AH  Ah  A�  A�  A�  A�  A�  A�  A�  A�  B  B
  B  B  B"  B*  B2  B:  BB  BJ  BR  BY��Bb  Bj  Br  Bz  B�  B�  B�  B�  B�  B�  B�  B���B���B�33B���B�  B�  B�  B�  B�  B�  B�  B�33B���B���B���B�  B�  B�  B�  B�  B�  B���B�  B�33B�  C � C� C� C� C� C
� C� C� C� C� C� C� C��C��C� C� C � C"� C$� C&� C(� C*� C,� C.� C0� C2� C4� C6� C8� C:� C<� C>� C@� CB� CD� CF� CH� CJ� CL� CN� CP� CR� CT� CV� CX� CZ� C\� C^� C`� Cb� Cd� Cf� Ch��Cj� Cl� Cn� Cp� Cr� Ct� Cv� Cx� Cz� C|� C~� C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�L�C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D�D��D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.��D/  D/� D0&fD0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDyFfD�)�D�Y�D���D���D�)�D�@ D��fD��fD�  D�VfD��3Dǳ3D�S3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A���A���A���A�A�  A�  A�A�A�A�A�A�%A�1A�
=A�
=A�1A�%A�
=A�JA�JA�JA�VA�VA�VA�JA�A���A���A���A��A��A��A��A��A��A��A��/A�bNA�XA�|�A��AͶFẠ�A���A�n�AȑhA�oA�p�A��A�1'A���A�1'A�-A��!A���A��+A�ƨA���A�r�A�A�A�dZA�?}A��HA���A��-A��9A�|�A�p�A��uA�C�A�jA��/A�l�A��A��A�`BA�^5A���A��FA��\A�A��HA��#A��A��`AoAy�^Aq%ApVAox�Aml�Ak|�Ag/A_�A\ZAZ�DAYAW/ASƨAPZAOC�AMl�AJ^5AI��AI;dAGAE;dAC7LAB��AA%A?C�A=�7A<A:JA7A5�FA3+A0�A,ĜA+ƨA(�jA'�7A'
=A#`BA!ƨA!�A r�A`BAM�A"�A�
A��A�AAƨAO�A%A%A��A^5AXA+A�FAĜA��AA��A`BA��Al�A�;A;dA\)A�A7LA�uA��A	�A	VA	dZA	�A��AS�AhsAM�A�mA�mA�AI�AƨA��A�A�AdZA/AoA��A��A��A�`A�+A��Ap�A|�A��A(�A�A?}A^5A��A ��A   @��y@���A 1'A -A {@�+@�G�@��9@��F@��@��@�K�@�b@��@���@��T@��H@�C�@�"�@��+@�/@�  @��P@��/@��@�C�@��-@�Ĝ@��@��@�ȴ@�~�@���@�%@�@�Q�@�|�@��T@�@���@��#@�-@�O�@��@�Q�@��m@�K�@�"�@�  @�9@�@�j@畁@��@�n�@��@�-@�M�@�R@��@�~�@���@�b@�ƨ@�ƨ@߅@�V@�@݁@�`B@�X@ݲ-@�$�@�~�@�M�@��T@��T@�X@�t�@�~�@��#@�?}@�V@���@؃@�C�@��y@�=q@Չ7@�7L@�Ĝ@ԃ@�1'@��
@�t�@�@ҟ�@�-@���@�r�@�Z@�Z@�(�@��
@θR@���@�O�@�G�@��@���@̋D@�A�@˶F@ˍP@ˍP@���@�ƨ@�dZ@ʰ!@�^5@���@ɡ�@�X@��@ȣ�@�ƨ@Ǿw@ǍP@�;d@�n�@�5?@Ų-@�%@��@���@Ĭ@�Q�@���@�K�@��@¸R@�@��/@� �@�S�@�~�@�J@��-@��@��@��@�bN@�9X@��@�"�@�ff@��@��-@�p�@�?}@���@��@�A�@��
@�ƨ@���@�K�@��!@�-@���@�`B@�Ĝ@�z�@���@�t�@�
=@��@���@��@�J@��@��-@�x�@��@�%@��@��j@�r�@�bN@�I�@��;@�o@��+@���@��7@�G�@�&�@�V@��`@���@���@�t�@�l�@�\)@�K�@�;d@���@�ff@�V@�5?@�@��T@�@��7@�X@��@���@���@��@�ƨ@���@�\)@�o@��!@�=q@�x�@��@�z�@�1'@� �@��@�  @��;@��@�S�@�
=@�ȴ@��H@��R@��+@��@���@��h@�x�@�hs@�?}@��@���@�I�@��F@�S�@��@��y@���@��^@�p�@���@��@�t�@��@��@��\@�M�@��@���@���@��h@�?}@�V@��/@���@���@��w@���@�S�@�+@���@�v�@�-@�{@���@��h@�x�@�V@�I�@��m@��@�t�@�"�@��R@�v�@�$�@�@���@���@���@��@�@�  @}O�@t�/@h��@_;d@Z�!@R-@J��@Dz�@<z�@6V@0��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111A���A���A���A���A���A�A�  A�  A�A�A�A�A�A�%A�1A�
=A�
=A�1A�%A�
=A�JA�JA�JA�VA�VA�VA�JA�A���A���A���A��A��A��A��A��A��A��A��/A�bNA�XA�|�A��AͶFẠ�A���A�n�AȑhA�oA�p�A��A�1'A���A�1'A�-A��!A���A��+A�ƨA���A�r�A�A�A�dZA�?}A��HA���A��-A��9A�|�A�p�A��uA�C�A�jA��/A�l�A��A��A�`BA�^5A���A��FA��\A�A��HA��#A��A��`AoAy�^Aq%ApVAox�Aml�Ak|�Ag/A_�A\ZAZ�DAYAW/ASƨAPZAOC�AMl�AJ^5AI��AI;dAGAE;dAC7LAB��AA%A?C�A=�7A<A:JA7A5�FA3+A0�A,ĜA+ƨA(�jA'�7A'
=A#`BA!ƨA!�A r�A`BAM�A"�A�
A��A�AAƨAO�A%A%A��A^5AXA+A�FAĜA��AA��A`BA��Al�A�;A;dA\)A�A7LA�uA��A	�A	VA	dZA	�A��AS�AhsAM�A�mA�mA�AI�AƨA��A�A�AdZA/AoA��A��A��A�`A�+A��Ap�A|�A��A(�A�A?}A^5A��A ��A   @��y@���A 1'A -A {@�+@�G�@��9@��F@��@��@�K�@�b@��@���@��T@��H@�C�@�"�@��+@�/@�  @��P@��/@��@�C�@��-@�Ĝ@��@��@�ȴ@�~�@���@�%@�@�Q�@�|�@��T@�@���@��#@�-@�O�@��@�Q�@��m@�K�@�"�@�  @�9@�@�j@畁@��@�n�@��@�-@�M�@�R@��@�~�@���@�b@�ƨ@�ƨ@߅@�V@�@݁@�`B@�X@ݲ-@�$�@�~�@�M�@��T@��T@�X@�t�@�~�@��#@�?}@�V@���@؃@�C�@��y@�=q@Չ7@�7L@�Ĝ@ԃ@�1'@��
@�t�@�@ҟ�@�-@���@�r�@�Z@�Z@�(�@��
@θR@���@�O�@�G�@��@���@̋D@�A�@˶F@ˍP@ˍP@���@�ƨ@�dZ@ʰ!@�^5@���@ɡ�@�X@��@ȣ�@�ƨ@Ǿw@ǍP@�;d@�n�@�5?@Ų-@�%@��@���@Ĭ@�Q�@���@�K�@��@¸R@�@��/@� �@�S�@�~�@�J@��-@��@��@��@�bN@�9X@��@�"�@�ff@��@��-@�p�@�?}@���@��@�A�@��
@�ƨ@���@�K�@��!@�-@���@�`B@�Ĝ@�z�@���@�t�@�
=@��@���@��@�J@��@��-@�x�@��@�%@��@��j@�r�@�bN@�I�@��;@�o@��+@���@��7@�G�@�&�@�V@��`@���@���@�t�@�l�@�\)@�K�@�;d@���@�ff@�V@�5?@�@��T@�@��7@�X@��@���@���@��@�ƨ@���@�\)@�o@��!@�=q@�x�@��@�z�@�1'@� �@��@�  @��;@��@�S�@�
=@�ȴ@��H@��R@��+@��@���@��h@�x�@�hs@�?}@��@���@�I�@��F@�S�@��@��y@���@��^@�p�@���@��@�t�@��@��@��\@�M�@��@���@���@��h@�?}@�V@��/@���@���@��w@���@�S�@�+@���@�v�@�-@�{@���@��h@�x�@�V@�I�@��m@��@�t�@�"�@��R@�v�@�$�@�@���@���G�O�@��@�@�  @}O�@t�/@h��@_;d@Z�!@R-@J��@Dz�@<z�@6V@0��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;oBǮBǮBǮBǮBǮBǮBȴBȴBȴBȴBȴBȴBȴBǮBǮBǮBȴBȴBȴBȴBȴBȴBȴBȴBȴBȴBɺB��B��B��B��B��B��B�
B�
B�B�B�B�ZB	)�B
��B
�B.BE�BhsB�LBǮBɺB�oB�%B��B��BcTBr�B�3B  B(�B �BuB��B�
BB��BŢB��BǮB��BVBW
BdZBw�Bp�BI�B-B!�BhB
�B
��B
��B
�B
[#B
+B
oB	��B	�B	�;B	�#B	��B	�-B	�JB	�%B	~�B	o�B	[#B	8RB	hB	B��B�B�`B�
B��BŢB�wB�}BĜBǮB��B��B��B��B�#B�;B�NB�TB�`B�mB�mB�mB�B�fB�;B��B��BȴB��B�XB�?B�B��B��B��B��B�LB��B�B��B��B	B		7B��B��B��B��B��B��B��B	B	VB	=qB	B�B	@�B	O�B	O�B	VB	_;B	dZB	aHB	YB	K�B	M�B	ZB	]/B	W
B	N�B	[#B	t�B	s�B	y�B	z�B	z�B	~�B	�B	�DB	��B	��B	��B	��B	��B	��B	�B	�3B	�9B	�!B	�'B	�FB	��B	ƨB	ÖB	ƨB	�}B	�XB	�-B	�B	�9B	��B	ɺB	��B	��B	ƨB	��B	�wB	�dB	�jB	�}B	��B	ɺB	�B	�)B	�mB	��B
B
B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�mB	�HB	�#B	�#B	�5B	�;B	�;B	�/B	�)B	�#B	�)B	�ZB	�B	�B	��B	��B	�B	�B	�sB	�NB	�mB	�B	�B	�B	�sB	�`B	�`B	�`B	�`B	�ZB	�ZB	�ZB	�ZB	�fB	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
1B
1B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B

=B

=B

=B

=B

=B
DB
DB
JB
JB
PB
PB
PB
PB
PB
PB
PB
PB
PB
PB
PB
PB
VB
PB
PB
PB
VB
VB
\B
bB
bB
bB
\B
\B
\B
bB
bB
hB
bB
\B
bB
bB
bB
bB
bB
bB
bB
oB
uB
uB
oB
uB
uB
{B
�B
{B
{B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
 �B
&�B
-B
49B
8RB
<jB
B�B
I�B
N�B
S�B
W
B
\)B
aHB
e`111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111BǟBǝBǞBǝBǟBǝBȥBȣBȢBȣBȣBȣBȤBǝBǝBǜBȣBȧBȣBȢBȤBȤBȦBȣBȤBȣBɩB˷B̽B��B��B��B��B��B��B��B�B�B�HB	)�B
�`B
�B-�BEzBhKB�$BǆBɑB�EB��B�zB�_Bc+Br�B�	B��B(�B �BEB��B��B�cB��B�wBέBǃB�ZBU�BV�Bd,Bw�BpsBI�B,�B!�B;B
�YB
̢B
��B
��B
Z�B
*�B
FB	��B	�kB	�B	��B	мB	�B	�"B	��B	~�B	oxB	Z�B	8.B	DB	 �B��B�B�<B��BʟB�B�UB�ZB�{BǉBϻB��BͱB��B��B�B�'B�.B�8B�FB�EB�GB�iB�=B�B��BͩBȍB�\B�2B�B��B��B��B��B��B�%B��B�{B��B��B	�B		B��B��B��B��B��B��B��B	 �B	(B	=BB	B`B	@TB	O�B	O�B	U�B	_
B	d)B	aB	X�B	K�B	M�B	Y�B	\�B	V�B	N�B	Z�B	t�B	s�B	y�B	z�B	z�B	~�B	��B	�B	�[B	�WB	�eB	�zB	��B	��B	��B	��B	�B	��B	��B	�B	�OB	�uB	�^B	�tB	�JB	�#B	��B	��B	�B	�TB	ɆB	ʍB	ʎB	�sB	�MB	�DB	�0B	�7B	�GB	�UB	ɅB	��B	��B	�8B	��B
 �B
 �B	��B	��B	��B	��B	�mB	�aB	�]B	�OB	�MB	�OB	�[B	�[B	�ZB	�eB	�cB	�ZB	�UB	�FB	�6B	�B	��B	��B	�B	�B	�B	��B	��B	��B	��B	�#B	�FB	�}B	��B	�B	�~B	�kB	�;B	�B	�8B	�LB	�`B	�bB	�=B	�'B	�(B	�*B	�)B	�!B	�$B	�$B	�"B	�.B	�LB	�mB	�~B	�B	�wB	�B	�~B	�gB	�YB	�]B	�hB	�mB	�mB	�rB	�mB	�fB	�lB	�fB	�kB	�kB	�lB	�sB	�rB	�lB	�rB	�vB	�wB	�B	�B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�|B	�|B	�}B	�wB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B	��B
 �B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

B

B

B

B

B

B

B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
!B
(B
*B
*B
"B
"B
#B
,B
'B
/B
)B
#B
)B
'B
'B
(B
(B
*B
+B
4B
;B
9B
3B
:B
:B
AB
EB
>B
?B
GB
FB
GB
DB
@B
EB
MB
NB
MB
SB
RB
KB
YB
XB
WB
PB
ZB
WB
VB
^B
^B
cB
cB
lB
lB
lB
lB
mB
`B
aB
_B
_B
`B
^B
cB
jB
kB
kB
oB
sB
wB
uB
{B
zB
}B
|B
{B
}B
~B
|B
{B
}B
�G�O�B
 �B
&�B
,�B
3�B
8B
<.B
BSB
I~B
N�B
S�B
V�B
[�B
aB
e#111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.5 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071417362016080714173620160807141736  AO  ARCAADJP                                                                    20150415193721    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150415193721  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150415193721  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807141736  IP                  G�O�G�O�G�O�                