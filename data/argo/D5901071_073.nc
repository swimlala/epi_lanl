CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:11Z UW 3.1 conversion   
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
_FillValue                 �  A@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  TP   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ]�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ex   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  g`   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  o    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  v�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  x�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �\   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �l   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �p   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               IA   AO  20111130140801  20190522121826  1727_5046_073                   2C  D   APEX                            2143                            040306                          846 @�t N ��1   @�t ����@7��;dZ�d`A�7L1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @333@�  @�33A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL�CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D fD � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DBfDB�fDCfDC� DD  DD� DE  DE�fDF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DUfDU� DV  DV� DV��DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`fD`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dly�Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Dss3DyFf11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@333@�  @�33A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL�CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D fD � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DBfDB�fDCfDC� DD  DD� DE  DE�fDF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DUfDU� DV  DV� DV��DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`fD`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dly�Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Dss3DyFf11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AąA�-A���Aß�AËDA�z�A�p�A�n�A�hsA�`BA�`BA�^5A�VA�S�A�M�A�K�A�5?A��;A�/A��;A�5?A���A�^5A��A�ĜA��A��A���A��hA��DA�p�A�O�A�;dA�33A��A�bA��A���A�C�A���A�VA�^5A�~�A�v�A�I�A���A�33A�ƨA�(�A��FA��A��^A�5?A���A�$�A�ĜA��uA��TA�VA��RA��A��^A��mA�;dA��DA�S�A���A�^5A��jA�v�A���A�^5A��;A�;dA�l�A��-A�JA�1'A��-A��!A���A�jA�?}A��A�-A��7A��A�ĜA���A�ĜA��^A�A��\A���A��FA�$�A�dZA��A���A���A���A���A��FA���A�S�A�r�A��A�XA���A�33A��uA�-A�ĜA��A�;dA~��A|^5A{hsAz$�Ayt�Ax~�Av9XAux�Au/At�AtbNAs`BArVAp�RAoAn  Al�!Aj�Ai�TAiG�Ah��Ah�Ah�Ah��Ah��Ag�PAd�!Ac�TAc`BAb �A`{A\��AZ�HAYG�AXZAW��AU��AR�\AQ�APn�AN��AL=qAK�^AJ1'AH��AG/AE�AD��ADA�AD �AC�PABVA@�!A?�TA>�A>5?A<ȴA;?}A:1A9;dA8�A7�wA6�uA6A5��A4ȴA3��A2ĜA2A�A0�A/�#A.�9A-+A,$�A*=qA(�+A'�PA%�A#��A#|�A"ȴA!�
A!?}A v�A 1AG�A��A��A7LA��A�A{A��A��A1'A��A��AjAA�A�^A�\A�yA�-A`BA�uA��AK�A�`A~�A��A
�RA$�A�A��Ap�AoA��A��A�A?}A%A�A�RA�A��A\)A�A�\A�A ��@��
@��+@�A�@�$�@�O�@��@��j@��@�X@��
@�@�`B@@@�R@�$�@�@�l�@��@�x�@��@�u@���@�hs@��@�33@��@�Ĝ@ۅ@١�@�r�@�|�@��@�\)@�=q@�r�@�(�@�I�@��
@ͺ^@�A�@�1@�1@���@�@�r�@�A�@� �@���@��;@��y@�-@�p�@�%@���@�bN@�  @�V@��@��`@���@�;d@���@���@���@��@�j@��R@��j@�33@��@��-@���@�l�@�~�@�@���@� �@��m@��
@�ƨ@���@�;d@���@�-@�J@�@�hs@��@�Ĝ@�1'@�|�@���@���@�/@�V@���@���@��`@�bN@�b@��F@��@�\)@�+@�"�@��@�@��@���@���@��!@�n�@�{@�@�J@��@��^@�O�@�(�@��y@�@�G�@�Ĝ@�%@���@�1@��
@��P@�K�@�+@��R@�(�@�C�@��+@���@���@�-@�~�@��y@���@���@�S�@�ƨ@��w@���@��P@�\)@�;d@�o@��y@��@�ȴ@��R@�v�@�$�@�`B@�&�@�Ĝ@���@��D@�z�@�I�@�1'@�  @���@�K�@��@�n�@�V@�$�@���@�G�@��/@��@�;d@�C�@�\)@�;d@��@���@���@���@��!@�~�@�5?@�J@�@���@�`B@�/@�&�@�/@�&�@�V@�%@���@��@�Q�@�  @���@�|�@�l�@�C�@��@�n�@�^5@�M�@�-@��#@�x�@�G�@��@��/@��D@�Q�@���@��m@�1'@� �@��w@�|�@�"�@��H@���@��!@���@���@�v�@��@��^@���@��7@�hs@��@���@��@�Q�@�1'@��
@�l�@�33@��T11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AąA�-A���Aß�AËDA�z�A�p�A�n�A�hsA�`BA�`BA�^5A�VA�S�A�M�A�K�A�5?A��;A�/A��;A�5?A���A�^5A��A�ĜA��A��A���A��hA��DA�p�A�O�A�;dA�33A��A�bA��A���A�C�A���A�VA�^5A�~�A�v�A�I�A���A�33A�ƨA�(�A��FA��A��^A�5?A���A�$�A�ĜA��uA��TA�VA��RA��A��^A��mA�;dA��DA�S�A���A�^5A��jA�v�A���A�^5A��;A�;dA�l�A��-A�JA�1'A��-A��!A���A�jA�?}A��A�-A��7A��A�ĜA���A�ĜA��^A�A��\A���A��FA�$�A�dZA��A���A���A���A���A��FA���A�S�A�r�A��A�XA���A�33A��uA�-A�ĜA��A�;dA~��A|^5A{hsAz$�Ayt�Ax~�Av9XAux�Au/At�AtbNAs`BArVAp�RAoAn  Al�!Aj�Ai�TAiG�Ah��Ah�Ah�Ah��Ah��Ag�PAd�!Ac�TAc`BAb �A`{A\��AZ�HAYG�AXZAW��AU��AR�\AQ�APn�AN��AL=qAK�^AJ1'AH��AG/AE�AD��ADA�AD �AC�PABVA@�!A?�TA>�A>5?A<ȴA;?}A:1A9;dA8�A7�wA6�uA6A5��A4ȴA3��A2ĜA2A�A0�A/�#A.�9A-+A,$�A*=qA(�+A'�PA%�A#��A#|�A"ȴA!�
A!?}A v�A 1AG�A��A��A7LA��A�A{A��A��A1'A��A��AjAA�A�^A�\A�yA�-A`BA�uA��AK�A�`A~�A��A
�RA$�A�A��Ap�AoA��A��A�A?}A%A�A�RA�A��A\)A�A�\A�A ��@��
@��+@�A�@�$�@�O�@��@��j@��@�X@��
@�@�`B@@@�R@�$�@�@�l�@��@�x�@��@�u@���@�hs@��@�33@��@�Ĝ@ۅ@١�@�r�@�|�@��@�\)@�=q@�r�@�(�@�I�@��
@ͺ^@�A�@�1@�1@���@�@�r�@�A�@� �@���@��;@��y@�-@�p�@�%@���@�bN@�  @�V@��@��`@���@�;d@���@���@���@��@�j@��R@��j@�33@��@��-@���@�l�@�~�@�@���@� �@��m@��
@�ƨ@���@�;d@���@�-@�J@�@�hs@��@�Ĝ@�1'@�|�@���@���@�/@�V@���@���@��`@�bN@�b@��F@��@�\)@�+@�"�@��@�@��@���@���@��!@�n�@�{@�@�J@��@��^@�O�@�(�@��y@�@�G�@�Ĝ@�%@���@�1@��
@��P@�K�@�+@��R@�(�@�C�@��+@���@���@�-@�~�@��y@���@���@�S�@�ƨ@��w@���@��P@�\)@�;d@�o@��y@��@�ȴ@��R@�v�@�$�@�`B@�&�@�Ĝ@���@��D@�z�@�I�@�1'@�  @���@�K�@��@�n�@�V@�$�@���@�G�@��/@��@�;d@�C�@�\)@�;d@��@���@���@���@��!@�~�@�5?@�J@�@���@�`B@�/@�&�@�/@�&�@�V@�%@���@��@�Q�@�  @���@�|�@�l�@�C�@��@�n�@�^5@�M�@�-@��#@�x�@�G�@��@��/@��D@�Q�@���@��m@�1'@� �@��w@�|�@�"�@��H@���@��!@���@���@�v�@��@��^@���@��7@�hs@��@���@��@�Q�@�1'@��
@�l�@�33@��T11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBJBbBoBuBuB{B�B�B{B�B�B�B�B�B�B�B!�B<jBffB�-B��B%B �B,B8RB:^B:^B:^B;dB<jB<jB<jB;dB<jB=qB<jB;dB9XB33B)�B&�B�BuBVBBBB��B��B�B�B��B��B��B�B�B�B�B�mB�NB�BB�#B��B��BɺB�qB�FB�'B��B��B��B�{B�\B�=B�B}�Bu�BiyBT�B=qB2-B)�B%�B�BoB%B��B��B�B�)B��B�9B��B�uB�+B{�BXB?}B'�B{BB
�B
�)B
��B
ȴB
�^B
�B
��B
��B
��B
�VB
�7B
�B
� B
{�B
p�B
e`B
_;B
XB
Q�B
L�B
A�B
=qB
;dB
9XB
5?B
0!B
'�B
�B
{B
VB
+B
B	��B	��B	��B	��B	��B	��B	�B	�yB	�/B	�)B	�B	�
B	�dB	��B	�DB	~�B	u�B	w�B	u�B	\)B	K�B	C�B	:^B	.B	,B	$�B	�B	JB	B��B	B	B	B��B��B�B�B�`B�HB�/B�B�B�B�B�B�B�B��B��B��B��BƨBĜBĜBŢBĜB�RB�^B�FB�B�B��B��B��B��B��B��B��B��B��B�oB�DB�7B�JB�%B�B�B�B�B�B�B� B~�Bu�Bq�Bp�Bn�Bm�Bl�BjBiyBffBe`B`BB`BB_;B^5B_;B_;B^5B[#BYBYBYBXBYBZBZB[#BZBXBW
BT�BR�BS�BR�BR�BR�BP�BP�BP�BP�BO�BM�BM�BM�BT�BVBT�BT�BW
BW
BR�BK�BH�BE�BD�BB�B@�BB�B@�B?}B>wB<jB:^B;dB;dB@�BD�BI�BM�BK�BL�BO�BQ�BM�BH�BI�BJ�BK�BL�BL�BS�BZB\)B\)B]/B^5B_;B`BBbNBffBs�B|�B}�B}�B}�B{�Bx�Bs�Bo�Bl�Bn�Bo�Bp�Bu�By�B{�B� B�B�B�B�B�B�%B�7B�JB�PB�VB�hB�oB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�!B�-B�3B�RB�dB��BÖBŢBƨBǮB��BɺBɺB��B��B�)B�ZB�B��B��B��B��B��B�B�B�B�B�B�B�B��B��B��B	B	B	JB	hB	�B	�B	�B	�B	#�B	&�B	'�B	)�B	,B	1'B	2-B	8RB	9XB	=qB	?}B	B�B	B�B	D�B	F�B	I�B	J�B	L�B	M�B	O�B	P�B	P�B	Q�B	T�B	ZB	^5B	aHB	dZB	hsB	hsB	iyB	jB	jB	jB	k�B	m�B	o�B	o�B	p�B	s�B	t�B	u�B	v�B	v�B	w�B	y�B	y�B	{�B	~�B	�B	�B	�+B	�+B	�1B	�7B	�DB	�VB	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�-B	�3B	�3B	�9B	�FB	�RB	�^B	�dB	�jB	�}B	B	ĜB	��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BbBhBuBuBuB{B�B�B{B�B�B�B�B�B�B�B"�B?}Bl�B�FB��B	7B"�B.B9XB:^B:^B:^B;dB=qB=qB<jB;dB<jB=qB=qB<jB;dB5?B+B+B�B�BuBB+BBB��B��B��B��B��B��B�B�B�B�B�B�`B�NB�BB�B��B��B��B�RB�?B�'B��B��B��B�oB�VB�B�By�Bo�B^5BA�B49B+B'�B#�B�B1BB��B�B�NB�B�dB��B��B�DB�=BcTBH�B0!B�BPB
�B
�BB
��B
��B
��B
�B
��B
��B
��B
�bB
�DB
�B
�B
�B
w�B
hsB
cTB
ZB
T�B
S�B
C�B
>wB
<jB
;dB
8RB
33B
-B
"�B
�B
oB
JB
B	��B	��B	��B	��B	��B	��B	��B	�B	�;B	�5B	�/B	�)B	B	��B	�VB	�B	w�B	{�B	|�B	_;B	N�B	H�B	@�B	0!B	0!B	(�B	�B	\B	+B��B	B	+B	%B	B��B�B�B�yB�fB�HB�/B�)B�#B�B�B�B�B�B��B��B��B��BȴBɺBɺB��B�wB�wB�wB�3B�B�B��B��B��B��B��B��B��B��B��B�\B�VB�\B�+B�%B�%B�B�%B�B�B�B�Bx�Br�Br�Bq�Bn�Bm�Bk�Bk�BjBm�BaHBaHB`BB_;B`BB`BBaHB]/BZBYBZBZB[#B[#B[#B]/B^5BZBZBXBW
BXBT�BS�BS�BS�BS�BS�BR�BO�BP�BN�BM�BVBXBW
BW
BXBYBXBL�BK�BG�BF�BD�BB�BD�BC�BA�B@�B?}B>wB=qB>wBA�BD�BJ�BQ�BM�BM�BO�BVBS�BJ�BJ�BJ�BK�BL�BN�BT�B[#B]/B]/B^5B_;BbNB`BBbNBgmBv�B� B}�B}�B~�B|�B{�Bv�Bo�Bm�Bo�Bp�Br�Bv�Bz�B}�B�B�B�B�B�B�%B�+B�=B�JB�VB�\B�oB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�!B�-B�3B�XB�jB��BÖBŢBǮBȴB��B��B��B��B��B�)B�`B�B��B��B��B��B��B��B�B�B�B�B�B�B�B��B��B	B	B	JB	hB	�B	�B	�B	�B	#�B	&�B	'�B	)�B	,B	2-B	33B	8RB	:^B	=qB	?}B	B�B	B�B	D�B	F�B	J�B	K�B	M�B	N�B	O�B	P�B	Q�B	R�B	VB	\)B	_;B	aHB	dZB	hsB	iyB	iyB	jB	jB	jB	k�B	m�B	o�B	o�B	q�B	s�B	t�B	u�B	v�B	v�B	w�B	y�B	z�B	|�B	~�B	�B	�%B	�+B	�+B	�1B	�=B	�JB	�VB	�bB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�-B	�3B	�3B	�?B	�LB	�XB	�^B	�dB	�qB	��B	B	ĜB	��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446592012010314465920120103144659  AO  ARGQ                                                                        20111130140801  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130140801  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144659  IP                  G�O�G�O�G�O�                