CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:24Z UW 3.1 conversion   
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
_FillValue                 �  Kt   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  Mp   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  U\   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]H   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  _D   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  g0   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  i,   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  q   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  {    PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �0   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �4   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �8   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �<   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �@   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               xA   AO  20111130141910  20190522121826  1727_5046_120                   2C  D   APEX                            2143                            040306                          846 @԰���1   @԰Ǧ��@6�KƧ��dl�C��1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @&ff@y��@���A   AffA@  A`  A���A�  A�  A�  A���A�  A�  A�33A�33B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  BhffBp  Bx  B�33B�  B�  B�  B�  B�  B�  B�33B�33B�33B�33B�  B���B���B���B���B���B�  B�  B���B���B�  B�  B�33B�  B���B�  B�  B�  B���B�  B�33C   C  C�C  C�fC
  C  C  C�C  C  C  C  C�C  C  C   C!�fC$  C&  C'�fC*  C,  C-�fC0  C2  C4�C6�C8  C9�fC<  C>�C@  CB  CD  CF  CH�CJ  CK�fCN  CP�CR  CT  CV  CX  CY�fC\  C^  C`  Cb  Cc�fCf  Ch�Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|�C~�C��C��C��C��C��C��C��C�  C�  C��C�  C�  C��C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C��C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C��3C��3C�  C�  C�  C��C��C��3C��3C��C�  C�  C�  C��3C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C��C�  C�  C��C�  C�  C��3C��3C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C��C�  C�  C��C��C�  C��3D � D  D�fD  D� D  Dy�D  D�fD  Dy�D��Dy�D  D� D  Dy�D��D	� D
fD
�fDfD�fDfD�fDfD� D  D� D  D�fD  Dy�D��D� D  D� D  Dy�D  D� D  D�fDfD� D  Dy�D  D�fDfD�fDfD� D��Dy�D��Dy�D��D� D  D� D  D�fD fD �fD!  D!y�D"  D"� D#  D#� D$fD$� D$��D%� D&  D&� D'  D'y�D(  D(�fD)  D)� D*  D*� D+  D+y�D+��D,y�D,��D-y�D.  D.� D/  D/�fD0fD0�fD1fD1� D1��D2� D3  D3� D4fD4�fD5fD5�fD6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=fD=�fD>  D>� D?  D?� D@  D@� DA  DA� DA��DB� DCfDC� DD  DD� DD��DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL�fDMfDM�fDN  DNy�DN��DO� DP  DP� DQ  DQ� DR  DR� DSfDS� DT  DT� DUfDU�fDVfDV�fDWfDW� DX  DX� DY  DY�fDZ  DZ� D[  D[y�D\  D\� D]  D]y�D^  D^� D^��D_� D`  D`� D`��Da� Db  Db� Dc  Dc� Dd  Dd� De  Dey�Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� DofDo� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dy� D�)�D�l�D��fD���D�  D�ffD���D��fD�  D�P D���D��3D�&fD�ffDڬ�D�ٚD�3D�` D� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @&ff@y��@���A   AffA@  A`  A���A�  A�  A�  A���A�  A�  A�33A�33B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  BhffBp  Bx  B�33B�  B�  B�  B�  B�  B�  B�33B�33B�33B�33B�  B���B���B���B���B���B�  B�  B���B���B�  B�  B�33B�  B���B�  B�  B�  B���B�  B�33C   C  C�C  C�fC
  C  C  C�C  C  C  C  C�C  C  C   C!�fC$  C&  C'�fC*  C,  C-�fC0  C2  C4�C6�C8  C9�fC<  C>�C@  CB  CD  CF  CH�CJ  CK�fCN  CP�CR  CT  CV  CX  CY�fC\  C^  C`  Cb  Cc�fCf  Ch�Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|�C~�C��C��C��C��C��C��C��C�  C�  C��C�  C�  C��C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C��C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C��3C��3C�  C�  C�  C��C��C��3C��3C��C�  C�  C�  C��3C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C��C�  C�  C��C�  C�  C��3C��3C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C��C�  C�  C��C��C�  C��3D � D  D�fD  D� D  Dy�D  D�fD  Dy�D��Dy�D  D� D  Dy�D��D	� D
fD
�fDfD�fDfD�fDfD� D  D� D  D�fD  Dy�D��D� D  D� D  Dy�D  D� D  D�fDfD� D  Dy�D  D�fDfD�fDfD� D��Dy�D��Dy�D��D� D  D� D  D�fD fD �fD!  D!y�D"  D"� D#  D#� D$fD$� D$��D%� D&  D&� D'  D'y�D(  D(�fD)  D)� D*  D*� D+  D+y�D+��D,y�D,��D-y�D.  D.� D/  D/�fD0fD0�fD1fD1� D1��D2� D3  D3� D4fD4�fD5fD5�fD6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=fD=�fD>  D>� D?  D?� D@  D@� DA  DA� DA��DB� DCfDC� DD  DD� DD��DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL�fDMfDM�fDN  DNy�DN��DO� DP  DP� DQ  DQ� DR  DR� DSfDS� DT  DT� DUfDU�fDVfDV�fDWfDW� DX  DX� DY  DY�fDZ  DZ� D[  D[y�D\  D\� D]  D]y�D^  D^� D^��D_� D`  D`� D`��Da� Db  Db� Dc  Dc� Dd  Dd� De  Dey�Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� DofDo� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dy� D�)�D�l�D��fD���D�  D�ffD���D��fD�  D�P D���D��3D�&fD�ffDڬ�D�ٚD�3D�` D� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�bNA�bNA�dZA�O�A� �A���A���A�VA��A�;dA�jA��+A��A��hA���A���A���A��!A��9A��FA��^A��jA��wA�ƨA�ƨA�ȴA���A��#A��;A��HA��A��A��A���A���A���A��A��#A�M�A�%A���A��A�$�A�M�A�I�A�E�A�XA��FA�z�A�ƨA�G�A���A��A�bA��7A�;dA���A�\)A��A���A��A��jA��A�M�A�t�A��-A�Q�A���A���A��PA��/A���A�5?A�jA�oA��9A�$�A�n�A���A��DA�-A���A��#A��jA��wA��wA�JA�33A�G�A��+A��!A��TA��A��A�E�A��`A�|�A�E�A�|�A��A�{A��A��A~��A|�jAxĜAv��As��Ao�AmC�AlĜAjĜAf=qAd�`A`�yA_`BA_/A^�A^$�A\�A\E�AY�FAYXAYVAW��AW+AV�9AVE�AU��AQ�wAN-AM��AM��AM��AM�hAL�uAK��AJ�AH�9AG
=AFAD�AC��AAS�A@�jA@$�A>�jA;��A:�jA9�PA8��A8M�A7��A7C�A6jA6{A5��A5XA4v�A2��A0�A0E�A/�mA.ZA+��A*��A)��A(�yA(�9A(�\A'��A&�\A&9XA&5?A&1'A%G�A$I�A$�A#`BA"ffA �A JAp�A�\A�PAn�A��AXAr�A7LAbA�DA�AA�Ax�A��A��A �A�^A/A�DA"�A�A?}A
�jA
ZA	p�A�A��A��A�A��A\)AS�AO�AoA��AbNA�7A^5A�A�A��A ��@�\)@���@�@�G�@�z�@��@��y@�-@�7L@�  @�dZ@�dZ@��^@���@��@�O�@�(�@�\)@�7@�h@�u@�(�@�t�@���@އ+@܋D@���@�hs@�Z@�dZ@թ�@��@�V@�A�@��
@�|�@���@�ff@��#@�=q@�7L@���@̬@��@�S�@�v�@��@ǥ�@�=q@�1'@�1'@�1'@�@��T@�@��@���@��@�l�@�M�@���@���@�hs@��m@���@�M�@�J@���@��P@�"�@��@���@�v�@�M�@��@���@��@��9@��@�|�@�C�@���@��^@���@�p�@���@���@�A�@�b@��F@�;d@��@��R@�M�@�J@���@��@�G�@�V@��j@�I�@�b@�|�@�"�@�@���@��R@��\@��@�I�@�33@��R@�V@�p�@��@��@�9X@�  @��m@���@���@�l�@�"�@���@��+@�V@�V@�=q@�{@��^@���@� �@�\)@��\@��@���@�`B@���@���@�z�@�1'@���@���@�dZ@�33@��@�n�@��#@���@�p�@�X@��@��@��j@�j@�I�@���@�;d@���@�5?@�J@���@��@�@�p�@�%@��@��/@��9@��D@�Z@�  @��
@�ƨ@��@�K�@�33@�+@�"�@���@���@�E�@��-@���@��h@��@�hs@�7L@���@���@��`@��@��@�j@�Z@�I�@�1'@��;@��w@���@�K�@�"�@�@��@�ȴ@��+@��+@�v�@�v�@�ff@�-@���@�@��#@���@��h@�p�@�G�@��@�1@��@�S�@�S�@�"�@�"�@�+@�33@��@�
=@��R@��^@�7L@��@�r�@�Q�@�bN@��;@�\)@�@���@�M�@���@��@��@���@��@�`B@�Z@�I�@��u@��9@���@�Ĝ@�1'@���@���@��@�C�@��@��@���@���@�M�@}��@t��@o\)@g�@[�
@S�
@K��@D��@>{@:-@4�@.�R@)��@&E�@!�@^5@j@bN@�F@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�bNA�bNA�dZA�O�A� �A���A���A�VA��A�;dA�jA��+A��A��hA���A���A���A��!A��9A��FA��^A��jA��wA�ƨA�ƨA�ȴA���A��#A��;A��HA��A��A��A���A���A���A��A��#A�M�A�%A���A��A�$�A�M�A�I�A�E�A�XA��FA�z�A�ƨA�G�A���A��A�bA��7A�;dA���A�\)A��A���A��A��jA��A�M�A�t�A��-A�Q�A���A���A��PA��/A���A�5?A�jA�oA��9A�$�A�n�A���A��DA�-A���A��#A��jA��wA��wA�JA�33A�G�A��+A��!A��TA��A��A�E�A��`A�|�A�E�A�|�A��A�{A��A��A~��A|�jAxĜAv��As��Ao�AmC�AlĜAjĜAf=qAd�`A`�yA_`BA_/A^�A^$�A\�A\E�AY�FAYXAYVAW��AW+AV�9AVE�AU��AQ�wAN-AM��AM��AM��AM�hAL�uAK��AJ�AH�9AG
=AFAD�AC��AAS�A@�jA@$�A>�jA;��A:�jA9�PA8��A8M�A7��A7C�A6jA6{A5��A5XA4v�A2��A0�A0E�A/�mA.ZA+��A*��A)��A(�yA(�9A(�\A'��A&�\A&9XA&5?A&1'A%G�A$I�A$�A#`BA"ffA �A JAp�A�\A�PAn�A��AXAr�A7LAbA�DA�AA�Ax�A��A��A �A�^A/A�DA"�A�A?}A
�jA
ZA	p�A�A��A��A�A��A\)AS�AO�AoA��AbNA�7A^5A�A�A��A ��@�\)@���@�@�G�@�z�@��@��y@�-@�7L@�  @�dZ@�dZ@��^@���@��@�O�@�(�@�\)@�7@�h@�u@�(�@�t�@���@އ+@܋D@���@�hs@�Z@�dZ@թ�@��@�V@�A�@��
@�|�@���@�ff@��#@�=q@�7L@���@̬@��@�S�@�v�@��@ǥ�@�=q@�1'@�1'@�1'@�@��T@�@��@���@��@�l�@�M�@���@���@�hs@��m@���@�M�@�J@���@��P@�"�@��@���@�v�@�M�@��@���@��@��9@��@�|�@�C�@���@��^@���@�p�@���@���@�A�@�b@��F@�;d@��@��R@�M�@�J@���@��@�G�@�V@��j@�I�@�b@�|�@�"�@�@���@��R@��\@��@�I�@�33@��R@�V@�p�@��@��@�9X@�  @��m@���@���@�l�@�"�@���@��+@�V@�V@�=q@�{@��^@���@� �@�\)@��\@��@���@�`B@���@���@�z�@�1'@���@���@�dZ@�33@��@�n�@��#@���@�p�@�X@��@��@��j@�j@�I�@���@�;d@���@�5?@�J@���@��@�@�p�@�%@��@��/@��9@��D@�Z@�  @��
@�ƨ@��@�K�@�33@�+@�"�@���@���@�E�@��-@���@��h@��@�hs@�7L@���@���@��`@��@��@�j@�Z@�I�@�1'@��;@��w@���@�K�@�"�@�@��@�ȴ@��+@��+@�v�@�v�@�ff@�-@���@�@��#@���@��h@�p�@�G�@��@�1@��@�S�@�S�@�"�@�"�@�+@�33@��@�
=@��R@��^@�7L@��@�r�@�Q�@�bN@��;@�\)@�@���@�M�@���@��@��@���@��@�`B@�Z@�I�@��u@��9@���@�Ĝ@�1'@���@���@��@�C�@��@��@���@���@�M�@}��@t��@o\)@g�@[�
@S�
@K��@D��@>{@:-@4�@.�R@)��@&E�@!�@^5@j@bN@�F@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBz�Bz�Bz�By�Bw�Bu�Bs�Bp�Bv�B�B�hB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�'B�-B�3B�9B�?B�FB�FB�?B�9B�-B�B��B��B��B�B�B��B��B�+Bx�Bp�Bo�Bx�B�%B�%B� By�Bv�Br�BhsBaHB[#BVBH�B?}B49B,B�B�B�BuB�B{B�B1B��B�yB�HB�5B��BĜB�XB�-B�B�\B~�BjBP�BF�BF�B8RB-B�BB%BB
��B
�sB
��B
�?B
��B
�VB
ffB
J�B
:^B
(�B
�B
VB	��B	�B	�5B	�}B	�B	��B	�PB	XB	I�B	"�B	�B	�B	�B	oB	JB	+B	B	#�B	'�B	%�B	(�B	#�B	�B	�B	JB��B��B��B��B��B��B�B�mB�#B��BɺB��B�FB��B��B��B��B�oB�bB�oB�bB�\B�PB�JB�JB�DB�7B�+B�%B�1B�hB�bB�{B�DBy�Bx�B�B�B�+B�JB�uB�uB��B��B��B��B��B��B��B��B�{B�oB�oB�\B�DB�1B�B�B� B~�B}�B~�Bz�Bz�Bz�Bw�B{�Bz�Bx�Bw�Bu�Bs�Bp�Bo�Bo�Bn�Bm�Bl�BjBhsBhsBgmBffBffBiyBo�Bq�Bo�Bn�Bk�BjBhsBffBe`Be`Be`BffBgmBgmBffBe`BdZBdZBe`Be`BhsBiyBl�Bn�Bp�Bn�BgmB^5BS�BQ�BP�BN�BJ�BF�BF�BC�BF�BH�BF�BI�BH�BK�BN�BQ�BR�B[#BaHBcTBhsBo�Bp�Br�Bx�B~�B�B�7B�7B�1B�=B�PB�\B��B��B��B��B��B��B�B�B�B�!B�B�!B�-B�-B�-B�^B��BĜBŢBǮBȴBȴB��B��B��B��B�
B�/B�5B�TB�yB�yB�B�B�B�B�B�B��B��B��B��B��B	  B	B	B	B	B	%B	+B	
=B	DB	DB	JB	DB	
=B	\B	uB	�B	�B	�B	�B	�B	#�B	%�B	&�B	'�B	'�B	(�B	)�B	,B	-B	.B	1'B	1'B	6FB	7LB	9XB	=qB	A�B	D�B	E�B	H�B	L�B	M�B	O�B	P�B	P�B	Q�B	T�B	VB	W
B	XB	ZB	^5B	bNB	dZB	e`B	e`B	gmB	gmB	hsB	jB	jB	l�B	o�B	q�B	r�B	s�B	t�B	t�B	u�B	w�B	{�B	}�B	~�B	� B	�B	�B	�B	�B	�%B	�7B	�DB	�JB	�JB	�DB	�PB	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�'B	�-B	�9B	�9B	�9B	�9B	�3B	�3B	�FB	�qB	��B	��B	B	B	B	ÖB	ÖB	ÖB	ĜB	ÖB	ĜB	ŢB	ƨB	ǮB	ǮB	ǮB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�#B	�#B	�/B	�;B	�BB	�BB	�NB	�TB	�TB	�TB	�TB	�ZB	�sB	��B
+B
\B
�B
&�B
0!B
6FB
<jB
A�B
F�B
K�B
P�B
VB
[#B
bNB
gmB
l�B
r�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Bz�Bz�B{�Bz�By�Bx�Bw�Bo�Bu�B� B�bB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�'B�-B�3B�9B�?B�FB�FB�FB�FB�XB�}BÖB�RB�dB�?B�LB�FB�B�{B�=B|�Bv�B� B�PB�bB�7B� B{�By�Bm�BiyBiyBe`BS�BM�BE�B?}B"�B�B�B�B�B �B%�B�B��B�B�mB�fB�#B��B�wB�dB�dB��B�hB� B]/BQ�BR�BF�BB�B,BDBJB
=B1B
��B
�TB
ŢB
�B
�B
w�B
ZB
F�B
2-B
.B
&�B
DB
	7B	��B	��B	�LB	�RB	��B	ffB	`BB	+B	�B	�B	�B	�B	{B	�B	%B	'�B	/B	,B	.B	)�B	)�B	2-B	�B	B��B��B��B	B��B��B��B�`B�B��B��BÖB�B�B�B��B��B��B��B�{B�uB�hB�hB�\B�VB�VB�\B�oB��B�{B��B��B��B�B~�B�+B�+B�=B�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�VB�7B�=B�7B�7B�7B�+B�B�B�B�B�B~�B}�B}�B� B{�Bu�Bs�Bs�Bu�Bt�Bs�Bp�Bn�Bk�BiyBgmBgmBk�Bs�Bu�Bv�Bu�Bm�Bl�Bk�Bn�Bl�BgmBhsBiyBjBk�BiyBhsBhsBhsBhsBhsBo�Br�Bo�Br�Bv�Bw�By�BiyBXBT�BVBXBS�BM�BL�BH�BJ�BM�BM�BR�BN�BN�BP�BQ�BVB]/BcTBcTBk�Bo�Br�Br�Bx�B~�B�+B�VB�VB�1B�=B�VB�\B��B��B��B��B��B��B�B�!B�!B�-B�B�9B�-B�-B�LB�qBBŢBŢBȴBɺBɺB��B��B��B��B�
B�5B�5B�TB�yB�yB�B�B�B�B�B�B��B��B��B��B��B	  B	B	B	B	%B	+B		7B	DB	DB	JB	JB	PB	\B	oB	�B	�B	�B	�B	 �B	!�B	$�B	&�B	&�B	'�B	'�B	(�B	)�B	,B	.B	/B	1'B	1'B	6FB	7LB	9XB	@�B	A�B	G�B	G�B	J�B	L�B	M�B	O�B	P�B	Q�B	R�B	VB	W
B	XB	YB	\)B	`BB	cTB	e`B	ffB	ffB	hsB	hsB	hsB	jB	l�B	n�B	q�B	s�B	s�B	s�B	t�B	u�B	u�B	x�B	{�B	}�B	� B	�B	�B	�B	�%B	�B	�+B	�=B	�DB	�JB	�JB	�JB	�VB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�-B	�3B	�9B	�9B	�?B	�?B	�3B	�?B	�FB	�qB	��B	��B	B	B	B	ÖB	ĜB	ŢB	ŢB	ĜB	ŢB	ŢB	ƨB	ȴB	ȴB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	�B	�
B	��B	��B	�B	�#B	�#B	�)B	�/B	�;B	�BB	�HB	�NB	�TB	�TB	�TB	�ZB	�ZB	�sB	��B
+B
\B
�B
&�B
0!B
6FB
<jB
A�B
F�B
K�B
Q�B
VB
[#B
bNB
gmB
l�B
r�B
u�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<��
<��<�o<�t�<#�
<D��<D��<D��<T��<�C�<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B<u<49X<e`B<�C�<���<#�
<#�
<#�
<#�
<#�
<D��<�o<�o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�o<e`B<�t�<�1<D��<49X<D��<e`B<�1<���<#�
<#�
<#�
<T��<�t�<���<�o<�o=o<�C�<u<D��<#�
<�C�<ě�<u<�9X<�/<49X<#�
<�t�<���<e`B<�9X<#�
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
<D��<���<���<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<#�
<#�
<#�
<49X<T��<#�
<#�
<T��<�C�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<T��<#�
<#�
<T��<u<#�
<#�
<#�
<#�
<#�
<#�
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
<49X<#�
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
<�t�<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447162012010314471620120103144716  AO  ARGQ                                                                        20111130141910  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130141910  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144716  IP                  G�O�G�O�G�O�                