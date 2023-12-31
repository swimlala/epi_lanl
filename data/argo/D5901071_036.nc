CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:01Z UW 3.1 conversion   
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
_FillValue                 �  Kl   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  Mh   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  UP   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]8   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  _4   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  g   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  i   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  q    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  x�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  z�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               $A   AO  20111130135853  20190522121825  1727_5046_036                   2C  D   APEX                            2143                            040306                          846 @�D<�z?�1   @�D=��@@7<�1&��c�A�7K�1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�33A�33B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR�CT�CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/�fD0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DBfDB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DKfDK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`y�Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do�fDp  Dp�fDq  Dq� Dr  Dr� Ds  Dsy�Dy��D��D�y�D���D��3D��D�` D���D��D� D�` D�� D�ٚD�  D�ffDږfD���D�,�D�L�D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�33A�33B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR�CT�CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/�fD0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DBfDB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DKfDK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`y�Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do�fDp  Dp�fDq  Dq� Dr  Dr� Ds  Dsy�Dy��D��D�y�D���D��3D��D�` D���D��D� D�` D�� D�ٚD�  D�ffDږfD���D�,�D�L�D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AсA�z�A�~�AсA�v�A�r�A�l�A�bNA�ZA�ZA�XA�S�A�VA�S�A�Q�A�Q�A�M�A�K�A�I�A�G�A�G�A�I�A�K�A�I�A�I�A�G�A�E�A�C�A�C�A�C�A�A�A�7LA�bA�ĜA�5?A�ĜA�bA�E�A�ƨA�&�A���A���A�1A���A��A�?}A�C�A��A�`BA�ZA�VA��A�\)A��A��A�jA�\)A��A��A���A�A�A�ZA�  A��A�-A�+A�M�A�9XA�9XA��TA�XA�M�A�+A��\A�bNA�1'A�%A���A�bA�-A�n�A�bA���A��DA�33A���A�ZA�1'A�dZA���A���A���A��\A�S�A�=qA�t�A���A��uA�ȴA�hsA�~�A���A��wA�;dA���A�A��A�O�A�FA~�A~=qA}��A|��AzVAyl�Ay7LAx�!Aw��AwO�Au`BAr��AoC�Al�Ail�Ag�;AfQ�Ae�Ae�7Ad��Ad9XAc��Ac+Ab  AahsAa�A`�A`^5A`-A_A]O�A[t�AZ��AZ �AY��AYC�AX~�AW��AWdZAVI�AUK�ATffATr�AT5?ASt�AR^5AP��APQ�AN��AMl�AK�;AJE�AI�AI7LAH�AHbNAFjAD9XAB��AA�A>-A=%A;;dA9��A8�A77LA6�A5ƨA5�A4jA3�wA2�A2ZA1\)A/��A.�jA.M�A-hsA,~�A+\)A*��A)ƨA(��A'"�A%p�A%�A#ƨA"��A!|�A �A bA&�A�\AI�Al�A�DA-A`BA�;A�uAbAVAI�A�#A��A��A�A/A�mA/AoA��A�+A�TA�AM�A�HA
��A	S�A~�A�mA�AQ�A��A;dAA�jA�TAoAffA�A��A ��@��w@���@��+@��#@��@��@���@�=q@��7@�j@�+@���@�9@�P@�{@�Ĝ@�l�@�M�@�hs@��@��@�7@��@�%@�9@�"�@݉7@��@��y@ٺ^@�/@��/@� �@�l�@�{@���@�~�@У�@�;d@�=q@˕�@��@�n�@��#@�hs@�ƨ@ũ�@Ĵ9@Å@��H@�?}@�\)@�{@�j@��H@��@���@���@��@���@���@��@�n�@�@���@�%@��/@�j@�Z@� �@��@�{@��/@��9@�z�@�9X@��@�+@���@�~�@��#@�|�@���@���@���@�Z@�l�@�
=@�
=@��H@��\@�-@�x�@�V@��@�|�@�=q@�{@���@�X@�7L@���@�@�ff@�M�@�V@�5?@�{@�G�@�Z@��@���@�V@��@�/@���@���@��@�bN@�  @�ƨ@���@���@��P@��@��P@��@�K�@�33@�"�@�@��H@�ȴ@�ȴ@���@���@��\@�V@�J@��@��-@��`@���@�I�@�l�@��@��R@�~�@�E�@�5?@�-@�-@�$�@�$�@�@��-@�X@�V@��`@���@���@��@�
=@��+@�ff@���@��h@�&�@���@�r�@�A�@��
@���@�|�@�\)@�;d@�o@��H@��\@���@�p�@�X@�&�@�V@���@���@�r�@�bN@�9X@��
@���@���@�dZ@�K�@���@���@�v�@�M�@���@��7@�?}@�/@���@��/@�j@��m@���@�l�@�"�@�@��y@���@��R@���@�ff@�^5@�5?@��#@���@��7@�O�@��9@�Z@�b@��m@�ƨ@�|�@�C�@��@��!@�M�@�=q@�{@���@���@��-@��h@�?}@�V@���@���@���@�r�@�1'@{�m@t��@lZ@e/@_
=@W�@Nȴ@I%@A��@=�@6�y@1��@,��@#C�@��@-@�j@�w@�
@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AсA�z�A�~�AсA�v�A�r�A�l�A�bNA�ZA�ZA�XA�S�A�VA�S�A�Q�A�Q�A�M�A�K�A�I�A�G�A�G�A�I�A�K�A�I�A�I�A�G�A�E�A�C�A�C�A�C�A�A�A�7LA�bA�ĜA�5?A�ĜA�bA�E�A�ƨA�&�A���A���A�1A���A��A�?}A�C�A��A�`BA�ZA�VA��A�\)A��A��A�jA�\)A��A��A���A�A�A�ZA�  A��A�-A�+A�M�A�9XA�9XA��TA�XA�M�A�+A��\A�bNA�1'A�%A���A�bA�-A�n�A�bA���A��DA�33A���A�ZA�1'A�dZA���A���A���A��\A�S�A�=qA�t�A���A��uA�ȴA�hsA�~�A���A��wA�;dA���A�A��A�O�A�FA~�A~=qA}��A|��AzVAyl�Ay7LAx�!Aw��AwO�Au`BAr��AoC�Al�Ail�Ag�;AfQ�Ae�Ae�7Ad��Ad9XAc��Ac+Ab  AahsAa�A`�A`^5A`-A_A]O�A[t�AZ��AZ �AY��AYC�AX~�AW��AWdZAVI�AUK�ATffATr�AT5?ASt�AR^5AP��APQ�AN��AMl�AK�;AJE�AI�AI7LAH�AHbNAFjAD9XAB��AA�A>-A=%A;;dA9��A8�A77LA6�A5ƨA5�A4jA3�wA2�A2ZA1\)A/��A.�jA.M�A-hsA,~�A+\)A*��A)ƨA(��A'"�A%p�A%�A#ƨA"��A!|�A �A bA&�A�\AI�Al�A�DA-A`BA�;A�uAbAVAI�A�#A��A��A�A/A�mA/AoA��A�+A�TA�AM�A�HA
��A	S�A~�A�mA�AQ�A��A;dAA�jA�TAoAffA�A��A ��@��w@���@��+@��#@��@��@���@�=q@��7@�j@�+@���@�9@�P@�{@�Ĝ@�l�@�M�@�hs@��@��@�7@��@�%@�9@�"�@݉7@��@��y@ٺ^@�/@��/@� �@�l�@�{@���@�~�@У�@�;d@�=q@˕�@��@�n�@��#@�hs@�ƨ@ũ�@Ĵ9@Å@��H@�?}@�\)@�{@�j@��H@��@���@���@��@���@���@��@�n�@�@���@�%@��/@�j@�Z@� �@��@�{@��/@��9@�z�@�9X@��@�+@���@�~�@��#@�|�@���@���@���@�Z@�l�@�
=@�
=@��H@��\@�-@�x�@�V@��@�|�@�=q@�{@���@�X@�7L@���@�@�ff@�M�@�V@�5?@�{@�G�@�Z@��@���@�V@��@�/@���@���@��@�bN@�  @�ƨ@���@���@��P@��@��P@��@�K�@�33@�"�@�@��H@�ȴ@�ȴ@���@���@��\@�V@�J@��@��-@��`@���@�I�@�l�@��@��R@�~�@�E�@�5?@�-@�-@�$�@�$�@�@��-@�X@�V@��`@���@���@��@�
=@��+@�ff@���@��h@�&�@���@�r�@�A�@��
@���@�|�@�\)@�;d@�o@��H@��\@���@�p�@�X@�&�@�V@���@���@�r�@�bN@�9X@��
@���@���@�dZ@�K�@���@���@�v�@�M�@���@��7@�?}@�/@���@��/@�j@��m@���@�l�@�"�@�@��y@���@��R@���@�ff@�^5@�5?@��#@���@��7@�O�@��9@�Z@�b@��m@�ƨ@�|�@�C�@��@��!@�M�@�=q@�{@���@���@��-@��h@�?}@�V@���@���@���@�r�@�1'@{�m@t��@lZ@e/@_
=@W�@Nȴ@I%@A��@=�@6�y@1��@,��@#C�@��@-@�j@�w@�
@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�BB\BVBDB  B��B�B�B�fB�;B��B��BÖB�FB�B��B�oB�VB�PB�7B�B{�Bu�Bo�BdZB^5BW
BR�BQ�BD�B8RB-B"�B&�B�BoBB�B�HB�5B�)B�B��BǮB�^B�!B��B��B�Bq�B_;BXBS�BF�B1'B(�B%�B$�B!�B�BuB1B
��B
�B
�mB
�#B
��B
B
�qB
��B
�=B
u�B
dZB
e`B
dZB
^5B
[#B
T�B
H�B
D�B
C�B
@�B
<jB
:^B
'�B
�B
	7B	�B	�sB	�;B	�B	��B	��B	��B	ɺB	ĜB	��B	�XB	�FB	�3B	�!B	�B	�B	��B	��B	�VB	�7B	�%B	�B	�B	{�B	x�B	u�B	q�B	k�B	l�B	t�B	}�B	� B	|�B	w�B	u�B	r�B	jB	bNB	[#B	XB	VB	R�B	M�B	B�B	6FB	.B	$�B	�B	JB	B��B��B�B�yB�sB�fB�`B�TB�;B�)B�B��B��B��BƨBB�}B�jB�XB�?B�-B�!B�B��B��B��B��B��B��B��B��B��B��B�{B�bB�DB�7B�+B�B�B� B}�B{�By�Bw�Bs�Bs�Bs�Bs�Bq�Bn�Bk�BffB`BB]/B[#BZBXBW
BW
BT�BS�BR�BP�BN�BM�BL�BK�BI�BG�BF�BE�BE�BD�BC�B@�B?}B?}B?}B>wB>wB>wB?}B?}B?}B=qB<jB:^B6FB6FB6FB5?B5?B49B2-B0!B0!B/B.B.B/B.B.B-B,B)�B,B,B,B)�B,B-B,B,B,B,B/B/B/B0!B/B/B0!B2-B5?B6FB9XB<jB@�BA�BA�BF�BG�BJ�BN�BR�BYBYBYBYB]/B_;Be`Bl�Bp�Br�Bt�Bt�Bt�Bt�Bv�B{�B~�B�B�B�+B�DB�hB�uB�uB��B��B��B��B��B��B��B��B�B�-B�?B�qB��B��B��B�
B�5B�TB�sB�B�B��B��B	  B	B	B	JB	oB	�B	�B	�B	�B	�B	�B	!�B	#�B	#�B	&�B	'�B	(�B	)�B	,B	,B	,B	-B	-B	.B	/B	0!B	1'B	2-B	7LB	7LB	7LB	9XB	:^B	?}B	@�B	A�B	A�B	A�B	A�B	@�B	@�B	@�B	A�B	C�B	E�B	F�B	G�B	K�B	M�B	O�B	S�B	S�B	XB	YB	[#B	_;B	aHB	bNB	e`B	gmB	hsB	iyB	iyB	jB	k�B	l�B	s�B	t�B	u�B	w�B	x�B	{�B	}�B	�B	�B	�B	�%B	�1B	�7B	�DB	�DB	�JB	�\B	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�-B	�3B	�FB	�LB	�RB	�dB	�dB	�jB	�wB	��B	ĜB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�)B	�/B	�5B	�;B	�BB	�HB	�mB	��B
1B
{B
�B
&�B
0!B
9XB
?}B
C�B
K�B
O�B
T�B
YB
\)B
aHB
gmB
l�B
q�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�BVB�B{BoBVB��B��B�B�B�`B��B��BɺB�^B�9B�B��B�hB�bB�JB�7B~�Bx�Bu�BgmBcTBYBXBVBH�B;dB1'B&�B,B!�B�B%B��B�NB�;B�/B�B�
B��B�qB�-B�B��B�=Bx�BaHBYBXBO�B49B+B&�B%�B"�B#�B�BVB
��B
�B
�B
�;B
�B
ŢB
ĜB
�9B
�VB
{�B
ffB
gmB
ffB
_;B
\)B
[#B
J�B
E�B
D�B
B�B
>wB
>wB
.B
!�B
hB	��B	�B	�TB	�B	�B	��B	��B	��B	ƨB	ĜB	�dB	�LB	�9B	�'B	�B	�B	�B	��B	�hB	�DB	�1B	�B	�B	}�B	z�B	x�B	t�B	n�B	l�B	u�B	� B	�B	�B	y�B	y�B	w�B	o�B	gmB	^5B	YB	W
B	T�B	T�B	I�B	:^B	2-B	2-B	�B	oB	+B��B��B�B�B�B�sB�mB�`B�HB�;B�B�B��B��BɺBŢB��B�wB�jB�^B�LB�'B�'B�B��B��B��B��B��B��B��B��B��B��B��B�bB�DB�DB�1B�B�B� B}�B}�B|�Bv�Bt�Bt�Bt�Bs�Bp�Bp�Bl�BgmBcTB^5B]/B[#BZBYBW
BT�BS�BS�BQ�BO�BM�BL�BK�BI�BG�BF�BF�BF�BF�BC�BA�B@�BA�B@�B@�B@�BA�BA�BA�B?}B>wB<jB<jB;dB8RB6FB5?B5?B5?B33B1'B33B0!B/B0!B0!B0!B/B.B/B/B/B.B/B.B.B-B-B/B0!B1'B1'B0!B0!B33B2-B33B49B6FB8RB:^B=qBA�BC�BD�BG�BH�BK�BN�BR�BZBYBZBZB_;BaHBe`Bm�Bq�Bs�Bu�Bu�Bu�Bu�Bz�B}�B�B�B�%B�7B�JB�hB�uB�{B��B��B��B��B��B��B��B��B�B�-B�?B�dB��B��B��B�
B�5B�TB�B�B�B��B��B	  B	B	%B	JB	uB	�B	�B	�B	�B	�B	�B	!�B	#�B	#�B	&�B	'�B	(�B	)�B	,B	,B	,B	-B	-B	.B	0!B	0!B	2-B	33B	8RB	8RB	8RB	9XB	;dB	?}B	@�B	A�B	A�B	A�B	A�B	@�B	@�B	A�B	B�B	D�B	E�B	G�B	H�B	L�B	N�B	O�B	S�B	T�B	YB	ZB	\)B	_;B	aHB	cTB	ffB	gmB	hsB	iyB	iyB	k�B	l�B	n�B	s�B	t�B	u�B	w�B	y�B	{�B	}�B	�B	�B	�B	�%B	�1B	�7B	�DB	�JB	�PB	�\B	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�-B	�3B	�FB	�LB	�XB	�dB	�jB	�qB	�}B	B	ŢB	ƨB	ǮB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�)B	�/B	�5B	�;B	�HB	�HB	�mB	��B
1B
{B
�B
&�B
0!B
9XB
?}B
C�B
K�B
O�B
T�B
YB
\)B
aHB
gmB
l�B
q�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446462012010314464620120103144646  AO  ARGQ                                                                        20111130135853  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130135853  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144646  IP                  G�O�G�O�G�O�                