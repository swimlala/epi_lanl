CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:09Z UW 3.1 conversion   
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
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K|   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  Mx   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  Uh   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]X   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  _T   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  gD   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  i@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  q0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  {   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �T   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �X   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �\   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �`   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �d   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               BA   AO  20111130140619  20190522121826  1727_5046_066                   2C  D   APEX                            2143                            040306                          846 @�j�A��	1   @�j��$ @7LI�^5?�c�z�G�1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@�  A   AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR�CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1�fD2  D2� D3fD3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DD��DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D\��D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Ds� Dy�3D�33D�vfD��fD��fD�&fD�l�D�� D��fD�&fD�\�D��3D�� D�,�D�i�Dڜ�D��3D�&fD�P D��D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@���@�  A   AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR�CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1�fD2  D2� D3fD3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DD��DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D\��D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Ds� Dy�3D�33D�vfD��fD��fD�&fD�l�D�� D��fD�&fD�\�D��3D�� D�,�D�i�Dڜ�D��3D�&fD�P D��D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�1'A�(�A�"�A��A��A��A�{A�bA���A���A���A���A���A���A��A��HA��RA���A��A�v�A�jA�hsA�`BA�^5A�ZA�Q�A�{A���A��yA��/A��wA���A�ffA�;dA��yA��uA�^5A���A��^A�n�A�%A��TA���A��A�r�A�$�A��;A���A���A�n�A��HA��7A�`BA�+A�A�n�A��RA��A�ƨA��-A��7A�^5A�C�A�+A��+A�"�A�K�A�A��A�1A�?}A�+A�jA���A���A�33A��uA��A��PA�-A���A��!A�ƨA���A�ZA�p�A�  A��A��wA��A�A�E�A�oA��A��+A�A�A�~�A�=qA��A���A�/A��hA��TA��A���A�=qA�ĜA�"�A���A�XA�&�A��A��PA�
=A��mA���A�  A�~�A�E�A�"�A�FA~Ay�^Au�AsƨAsS�Arr�Aq�;ApQ�An�Am��Al�+Aj��Aip�Ah�Ag&�AfJAc�-AbbA_S�A^  A\ȴA\r�A\-AY|�AX �AW�-AV��AV{AUt�AT�RAS��AS%AQ��AO�AN�AN�\AM��AM�AMG�AL1AK&�AJ{AH�jAH-AG�AE�AD��ADACO�AB9XA@A�A=A9�A8��A6�9A5�TA5+A3ƨA0��A.�HA.A-O�A,��A, �A+�-A+�A*�\A*I�A)�#A)`BA)&�A(�A'��A'hsA'&�A&�yA&r�A%�wA%33A#��A"ĜA!�;A!VA �A ~�A 1'A��Al�A/Az�AK�A�
AffA��A��AdZA�AK�A��AbNAbAoAVA��AC�A%A1'A�wA/AdZAS�A��A��AK�A
�yA
�+A	��A	`BA	�A�A33A(�A%Av�A^5AQ�A1A�PA�!@�K�@��@�%@�9X@�|�@��@�Ĝ@�bN@�Q�@���@�J@�33@��@�\@�X@�C�@�@��@��@�Z@�-@�b@�+@��@�-@݁@�Z@�J@�@�j@�Q�@߶F@�|�@�;d@�E�@��@�@�C�@ڰ!@���@�7L@݉7@�x�@�hs@ݙ�@�7L@�1'@��@١�@�hs@�A�@��H@�
=@���@Гu@� �@�Z@�j@�I�@�1@�|�@�E�@�dZ@ȴ9@�`B@���@�33@�M�@�`B@�z�@��@���@���@��F@�C�@�hs@��@��@��@���@���@�j@��;@�@��+@�^5@�/@��9@�z�@��@��F@��P@�"�@�E�@��h@�V@�1'@��@�\)@��H@��@�Q�@���@�-@�?}@���@��D@��@��@��w@��@�S�@�o@��+@�v�@�~�@�v�@��+@���@��@�=q@���@�b@���@�\)@���@��^@��`@�z�@�dZ@�J@�@���@��7@�x�@�/@���@��@�K�@�5?@��@�x�@�`B@�X@��`@��@���@��@�dZ@�33@���@�J@���@��T@��@���@�J@�J@�J@��#@��@�Q�@�l�@���@��@�@��R@�V@��^@�p�@���@��@�9X@��m@�S�@�5?@�@��@��7@���@��@�z�@�(�@��
@���@�dZ@�33@��@��R@���@��+@�{@��T@��-@��h@�?}@��`@�Ĝ@��j@��j@��9@��u@�z�@�1'@��;@��F@���@�|�@�K�@��y@�ȴ@�n�@�{@��T@��#@���@���@��^@���@�G�@�&�@��@���@���@���@�Z@��@�b@�1@��m@��
@��w@��@���@�\)@�@��H@��9@{33@pb@hQ�@a�@[ƨ@SS�@I�^@CdZ@?+@9X@2�@-�@(bN@"�@5?@�@��@�`@p�@	�#1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�1'A�(�A�"�A��A��A��A�{A�bA���A���A���A���A���A���A��A��HA��RA���A��A�v�A�jA�hsA�`BA�^5A�ZA�Q�A�{A���A��yA��/A��wA���A�ffA�;dA��yA��uA�^5A���A��^A�n�A�%A��TA���A��A�r�A�$�A��;A���A���A�n�A��HA��7A�`BA�+A�A�n�A��RA��A�ƨA��-A��7A�^5A�C�A�+A��+A�"�A�K�A�A��A�1A�?}A�+A�jA���A���A�33A��uA��A��PA�-A���A��!A�ƨA���A�ZA�p�A�  A��A��wA��A�A�E�A�oA��A��+A�A�A�~�A�=qA��A���A�/A��hA��TA��A���A�=qA�ĜA�"�A���A�XA�&�A��A��PA�
=A��mA���A�  A�~�A�E�A�"�A�FA~Ay�^Au�AsƨAsS�Arr�Aq�;ApQ�An�Am��Al�+Aj��Aip�Ah�Ag&�AfJAc�-AbbA_S�A^  A\ȴA\r�A\-AY|�AX �AW�-AV��AV{AUt�AT�RAS��AS%AQ��AO�AN�AN�\AM��AM�AMG�AL1AK&�AJ{AH�jAH-AG�AE�AD��ADACO�AB9XA@A�A=A9�A8��A6�9A5�TA5+A3ƨA0��A.�HA.A-O�A,��A, �A+�-A+�A*�\A*I�A)�#A)`BA)&�A(�A'��A'hsA'&�A&�yA&r�A%�wA%33A#��A"ĜA!�;A!VA �A ~�A 1'A��Al�A/Az�AK�A�
AffA��A��AdZA�AK�A��AbNAbAoAVA��AC�A%A1'A�wA/AdZAS�A��A��AK�A
�yA
�+A	��A	`BA	�A�A33A(�A%Av�A^5AQ�A1A�PA�!@�K�@��@�%@�9X@�|�@��@�Ĝ@�bN@�Q�@���@�J@�33@��@�\@�X@�C�@�@��@��@�Z@�-@�b@�+@��@�-@݁@�Z@�J@�@�j@�Q�@߶F@�|�@�;d@�E�@��@�@�C�@ڰ!@���@�7L@݉7@�x�@�hs@ݙ�@�7L@�1'@��@١�@�hs@�A�@��H@�
=@���@Гu@� �@�Z@�j@�I�@�1@�|�@�E�@�dZ@ȴ9@�`B@���@�33@�M�@�`B@�z�@��@���@���@��F@�C�@�hs@��@��@��@���@���@�j@��;@�@��+@�^5@�/@��9@�z�@��@��F@��P@�"�@�E�@��h@�V@�1'@��@�\)@��H@��@�Q�@���@�-@�?}@���@��D@��@��@��w@��@�S�@�o@��+@�v�@�~�@�v�@��+@���@��@�=q@���@�b@���@�\)@���@��^@��`@�z�@�dZ@�J@�@���@��7@�x�@�/@���@��@�K�@�5?@��@�x�@�`B@�X@��`@��@���@��@�dZ@�33@���@�J@���@��T@��@���@�J@�J@�J@��#@��@�Q�@�l�@���@��@�@��R@�V@��^@�p�@���@��@�9X@��m@�S�@�5?@�@��@��7@���@��@�z�@�(�@��
@���@�dZ@�33@��@��R@���@��+@�{@��T@��-@��h@�?}@��`@�Ĝ@��j@��j@��9@��u@�z�@�1'@��;@��F@���@�|�@�K�@��y@�ȴ@�n�@�{@��T@��#@���@���@��^@���@�G�@�&�@��@���@���@���@�Z@��@�b@�1@��m@��
@��w@��@���@�\)@�@��H@��9@{33@pb@hQ�@a�@[ƨ@SS�@I�^@CdZ@?+@9X@2�@-�@(bN@"�@5?@�@��@�`@p�@	�#1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B�B�
B�
B�
B�
B�
B�B�B�B�B�
B�
B�
B�B�B�B�B�B�
B�
B�
B�B��B��B�B�B�B�B�;B�TB�fB�yB�yB�yB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�sB�NB�5B�#B�B�B�B�B�
B�B��BB��BĜB��B�RB�B��B�hB�DB~�Bo�BffB]/BXBQ�BJ�B<jB2-B%�B�BDBB��B��B�/B��BƨBĜB��B�?B��B�bBy�B^5BI�B?}B6FB2-B�B�BbB
��B
��B
�B
�B
�B
�fB
�/B
��B
�B
��B
�7B
�B
�B
}�B
w�B
k�B
T�B
?}B
49B
1'B
,B
(�B
�B
{B
bB
+B
  B	��B	�B	�sB	�BB	��B	��B	�qB	�FB	�!B	�B	��B	��B	��B	�{B	�\B	�JB	�1B	�B	~�B	z�B	w�B	o�B	k�B	iyB	ffB	dZB	aHB	\)B	W
B	Q�B	M�B	K�B	G�B	@�B	;dB	7LB	2-B	+B	!�B	bB	  B��B�B�B�fB�BɺB��B�jB�jB�^B�RB�?B�9B�3B�-B�!B�B�B�B�!B�'B�'B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�oB�bB�PB�DB�=B�1B�B�B� B~�B|�By�Bw�Bu�Bt�Bs�Bq�Bo�Bk�BiyBdZBdZBcTBbNBaHB`BB_;B^5B\)BYBW
BT�BT�BT�BS�BR�BQ�BN�BI�BI�BH�BG�BF�BE�BD�BE�BH�BK�BK�BH�BG�BJ�BK�BM�BH�BC�B>wB<jB>wBC�BB�BD�BE�BE�BL�Be`Br�Bx�By�Bx�By�Bz�B}�B�=B�\B�\B�PB��B��B�-B�XB�dBŢB��B��B��B�B�B��BɺBɺB��B��B��B��B��B��B��B�
B�B��B��BĜBĜBĜBÖBĜBƨBȴB��B��B��B��B��B��B��B��B�
B�5B�/B�)B�/B�NB�yB�B�B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B	B	B	B	%B	1B	1B		7B	DB	JB	
=B		7B	bB	oB	�B	�B	�B	�B	�B	!�B	&�B	'�B	(�B	(�B	(�B	(�B	)�B	+B	,B	0!B	49B	5?B	5?B	5?B	6FB	;dB	?}B	?}B	@�B	A�B	C�B	J�B	K�B	O�B	Q�B	R�B	S�B	T�B	T�B	VB	XB	YB	[#B	\)B	_;B	bNB	ffB	hsB	hsB	hsB	gmB	hsB	hsB	gmB	hsB	m�B	o�B	o�B	q�B	t�B	v�B	w�B	x�B	z�B	z�B	{�B	|�B	}�B	~�B	�B	�B	�%B	�+B	�7B	�=B	�PB	�bB	�hB	�hB	�hB	�oB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�'B	�'B	�'B	�'B	�-B	�9B	�?B	�?B	�FB	�LB	�RB	�^B	�jB	�jB	�qB	�}B	��B	��B	B	B	ÖB	ŢB	ƨB	��B	�yB	��B
PB
�B
�B
'�B
2-B
;dB
C�B
H�B
O�B
S�B
ZB
`BB
e`B
jB
m�B
r�B
v�B
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B��B�B�
B�
B�
B�
B�
B�B�B�B�B�
B�B�
B�B�B�B�B�B�
B�
B�
B�
B��B��B�B�
B�B�#B�BB�`B�sB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�`B�HB�)B�B�#B�#B�B�
B�B��BƨB��BƨBÖB�jB�9B�B��B�{B�1Bs�BjB_;BZBT�BN�B?}B5?B+B�BJB1BB��B�`B��BǮBŢBB�dB��B��B�BgmBM�BB�B:^B8RB&�B�B�BB
��B
�B
�B
�B
�B
�B
ǮB
�FB
��B
�JB
�B
�B
� B
}�B
w�B
`BB
B�B
5?B
33B
-B
,B
#�B
�B
uB
DB
B	��B	�B	�B	�fB	�B	��B	��B	�XB	�'B	�B	�-B	��B	��B	��B	�hB	�VB	�DB	�+B	�B	~�B	~�B	r�B	l�B	k�B	gmB	ffB	e`B	_;B	[#B	VB	O�B	M�B	J�B	B�B	<jB	8RB	49B	/B	'�B	�B	B��B�B�B�B�HB��BÖB�wB�wB�jB�XB�LB�FB�9B�3B�-B�!B�!B�!B�'B�-B�-B�-B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�\B�JB�DB�DB�7B�B�B� B� B{�By�Bv�Bu�Bv�Bs�Bq�Br�Bp�BiyBe`Be`BdZBcTBcTBaHB_;B_;B^5B[#BYBW
BVBT�BS�BS�BS�BT�BK�BJ�BH�BG�BF�BG�BF�BH�BL�BN�BL�BH�BK�BM�BP�BM�BG�BB�B?}BA�BF�BD�BE�BF�BF�BH�BcTBs�Bz�Bz�By�Bz�B{�B� B�DB�bB�uB�VB��B��B�-B�XB�dBŢB��B��B�B�#B�;B��B��BɺB��B��B��B��B��B��B�B�B�B�B��BɺBƨBŢBŢBƨBȴBɺB��B��B��B��B��B��B��B��B�B�;B�5B�/B�;B�TB�B�B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B	  B	B	B	B	%B	1B	1B		7B	DB	PB	JB	
=B	hB	oB	�B	�B	�B	�B	�B	#�B	'�B	'�B	(�B	(�B	)�B	)�B	+B	,B	.B	1'B	49B	5?B	5?B	6FB	7LB	<jB	?}B	?}B	@�B	B�B	D�B	J�B	K�B	O�B	Q�B	R�B	S�B	T�B	VB	W
B	YB	[#B	\)B	\)B	_;B	cTB	gmB	iyB	iyB	iyB	hsB	iyB	iyB	hsB	jB	m�B	o�B	p�B	r�B	u�B	v�B	x�B	y�B	z�B	z�B	{�B	}�B	}�B	~�B	�B	�B	�%B	�+B	�7B	�DB	�VB	�bB	�hB	�hB	�hB	�oB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�'B	�'B	�'B	�'B	�3B	�9B	�?B	�?B	�FB	�LB	�XB	�^B	�jB	�jB	�qB	�}B	��B	��B	B	ÖB	ĜB	ŢB	ƨB	��B	�yB	��B
PB
�B
�B
'�B
2-B
;dB
C�B
I�B
O�B
S�B
ZB
`BB
e`B
jB
m�B
r�B
v�B
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<u<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446572012010314465720120103144657  AO  ARGQ                                                                        20111130140619  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130140619  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144657  IP                  G�O�G�O�G�O�                