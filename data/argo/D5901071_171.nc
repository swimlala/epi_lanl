CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:39Z UW 3.1 conversion   
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130143031  20190522121827  1727_5046_171                   2C  D   APEX                            2143                            040306                          846 @��S��1   @�� @5��;dZ�c���`A�1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�  @�  A��A   A>ffA`  A���A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B_��Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B���B���B���B�  B�  B���B���B���B���B�  B�33C   C  C  C�C  C
  C�fC  C�C  C�fC�fC�fC  C�C�C �C"�C$�C&�C(�C*�C,  C-�fC0  C2  C4�C6�C8  C9�fC<  C>�C@  CB  CD�CF�CH�CI�fCK�fCN  CP  CR  CT  CV  CX  CZ  C\�C^  C`  Cb  Cc�fCf�Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cu�fCx  Cz  C{�fC~  C�  C�  C��3C��3C��C�  C��3C�  C��C�  C��3C�  C�  C�  C��3C��3C��3C��3C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C��3C�  C��C��C�  C�  C�  C�  C�  C�  C��3C��3C�  C��C�  C�  C��3C��3C�  C��C�  C��3C�  C�  C�  C��3C�  C�  C��3C��3C�  C�  C�  C�  C��C�  C��3C�  C��C�  C�  C��C�  C�  C�  C�  C��C�  C��3C��3C��3C��3C��3C��3C�  C��C�  C�  C��C�  C�  C�  C��C��C��C��C��C�  C��3C��3C�  C��C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C��3C��3C��3C��3C��3C��3C��3C��3D   D � D  Dy�D  D� D��D� DfD� D��Dy�D��Dy�D  D� DfD� D	  D	� D
  D
� D  D� D  D� D  D� D��Dy�D��D� D  Dy�D  D� D  D�fD  D� D  Dy�D  D�fD  Dy�D��D� D  D� DfD�fD  Dy�D��D� D  D� D  D� D��Dy�D��Dy�D��D y�D ��D!� D"fD"�fD#  D#y�D#��D$� D%fD%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+y�D,  D,� D,��D-� D.  D.� D/  D/� D0fD0�fD1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6fD6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<fD<� D=  D=� D=��D>� D?fD?� D@  D@� DA  DAy�DB  DB� DB��DCy�DD  DD�fDEfDE� DF  DF� DG  DG� DH  DHy�DH��DIy�DI��DJy�DJ��DK� DL  DL� DM  DM� DN  DN� DO  DO� DO��DPy�DQ  DQ� DR  DR� DS  DSy�DT  DT� DU  DU� DV  DV� DW  DW�fDX  DX� DY  DY� DZ  DZ� D[fD[� D[��D\� D]  D]� D^  D^� D_  D_� D`  D`� D`��Da� Db  Db� DcfDc� Dc��Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dy��D�  D�l�D��fD��3D�#3D�s3D�� D��3D�33D�VfD��fD���D�  D�L�Dڃ3D��D��D�i�D�f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�ff@�ffA��A#33AA��Ac33A�ffA���A���A���A���Aљ�AᙚA�B ��B��B��B��B ��B(��B0��B8��B@��BH��BP��BX��B`ffBh��Bp��Bx��B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB���B�ffB�33B�ffB�ffB�ffB�ffB�ffB�ffB�33B�33B�33B�ffB�ffB�33B�33B�33B�33B�ffB���C 33C33C33CL�C33C
33C�C33CL�C33C�C�C�C33CL�CL�C L�C"L�C$L�C&L�C(L�C*L�C,33C.�C033C233C4L�C6L�C833C:�C<33C>L�C@33CB33CDL�CFL�CHL�CJ�CL�CN33CP33CR33CT33CV33CX33CZ33C\L�C^33C`33Cb33Cd�CfL�Ch33Cj33Cl33Cn33Cp33Cr33Ct33Cv�Cx33Cz33C|�C~33C��C��C��C��C�&fC��C��C��C�&fC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&fC��C��C��C��C��C��C��C�&fC�&fC��C��C��C��C��C��C��C��C��C�&fC��C��C��C��C��C�&fC��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&fC��C��C��C�&fC��C��C�&fC��C��C��C��C�&fC��C��C��C��C��C��C��C��C�&fC��C��C�&fC��C��C��C�&fC�&fC�&fC�&fC�&fC��C��C��C��C�&fC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&fC��C��C��C��C��C��C��C��C��D �D ��D�D�fD�D��DfD��D3D��DfD�fDfD�fD�D��D3D��D	�D	��D
�D
��D�D��D�D��D�D��DfD�fDfD��D�D�fD�D��D�D�3D�D��D�D�fD�D�3D�D�fDfD��D�D��D3D�3D�D�fDfD��D�D��D�D��DfD�fDfD�fD fD �fD!fD!��D"3D"�3D#�D#�fD$fD$��D%3D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+�fD,�D,��D-fD-��D.�D.��D/�D/��D03D0�3D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D63D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<3D<��D=�D=��D>fD>��D?3D?��D@�D@��DA�DA�fDB�DB��DCfDC�fDD�DD�3DE3DE��DF�DF��DG�DG��DH�DH�fDIfDI�fDJfDJ�fDKfDK��DL�DL��DM�DM��DN�DN��DO�DO��DPfDP�fDQ�DQ��DR�DR��DS�DS�fDT�DT��DU�DU��DV�DV��DW�DW�3DX�DX��DY�DY��DZ�DZ��D[3D[��D\fD\��D]�D]��D^�D^��D_�D_��D`�D`��DafDa��Db�Db��Dc3Dc��DdfDd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dy��D�&fD�s3D���D���D�)�D�y�D��fD���D�9�D�\�D���D��3D�&fD�S3Dډ�D�� D�3D�p D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�K�A�M�A�M�A�O�A�O�A�O�A�O�A�M�A�G�A�K�A�O�A�O�A�Q�A�Q�A�K�A�K�A�S�A�XA�ZA�XA�\)A�^5A�^5A�`BA�`BA�`BA�bNA�dZA�G�Aҧ�A���A�A�A��A���A��HA�VAʍPA�-A�|�A��yA+A��!A��wA��A��A�z�A�ZA���A�%A�33A�~�A�ƨA��7A�M�A���A��^A��A���A���A�l�A�S�A�$�A�%A��A�=qA�x�A�O�A�+A��A��FA�G�A��/A���A�ȴA�?}A���A�ZA�+A�z�A�-A�9XA�ĜA���A��+A�S�A��!A�7LA�1A���A��A���A���A�E�A���A��A�1A�n�A���A��A��A�bA�ffA�&�A�^5A�Q�A���A�
=A�r�A�+A�
=A���A��A��/A���A�bNA��+A��A�K�A�JA��mA�A�  A~�A|5?Ay
=Au�FAt��Aq�Al �Af�+Ac�wAb5?Aa|�A_��A]�hA\AX�9AW33AV�AV~�AR5?AMp�AL��AK�hAKAI�AG�^AF5?AD�`ADI�AC�ACAC�AB1'A@�yA@�A=��A<�+A<1A;C�A8ȴA8JA7�-A7�A6��A6r�A5��A3l�A1O�A/p�A.bNA-��A-A,�+A,�A+x�A)\)A'�A'"�A%�A$�A#��A"��A"�\A"A!��A!&�A �+A =qAA��A��A��Az�AQ�A(�AA�A�jA�A��A|�AM�A��AS�AVA�AXA��A5?A
1A	l�A��A�^A��A�wA�/AJA�;A��A �@�Ĝ@��@�33@�"�@�o@�
=@�@���@��@�  @�/@�ƨ@�Z@��H@�@睲@�M�@�@��@�@�G�@�K�@�$�@���@ۍP@�^5@�O�@���@��@���@��@�j@� �@�^5@ѡ�@�G�@�I�@�ƨ@�"�@�`B@�%@�j@��
@�"�@ʧ�@�~�@�@�hs@�?}@��@�V@���@�b@�l�@�33@�E�@�r�@�C�@�?}@�\)@���@�J@�p�@��@��j@��@���@�Z@�9X@� �@�1@��@��;@��;@��;@��;@��
@���@��@���@���@�"�@�5?@���@�O�@��`@���@��j@���@�z�@�bN@�Q�@�A�@�1@��@�S�@���@�^5@�n�@���@���@���@�r�@��@��@�dZ@�C�@�ȴ@��@�bN@��@���@�|�@�|�@�;d@��@�~�@�E�@�5?@���@���@���@�j@���@���@��;@��;@��w@��@�\)@�
=@��@��@��R@��H@���@�^5@�M�@�5?@�$�@��T@�x�@�`B@�`B@��7@�`B@��`@��@�(�@�ƨ@�t�@�@�ff@��@��@���@��@�z�@�b@���@�t�@�33@��@��H@��R@��+@�n�@�=q@�@���@��-@���@��7@�O�@��@���@�Ĝ@���@�I�@��;@��w@���@��@�\)@�;d@�o@���@���@��y@��@��@�ȴ@���@���@�&�@���@���@��9@���@���@��@�bN@�bN@��@�C�@�v�@�{@���@��T@���@��^@���@���@��7@�x�@�`B@�/@��@�V@��/@�bN@��;@�S�@�~�@�x�@�hs@�`B@�G�@�/@��@��`@���@���@��9@��D@�  @��P@�+@��y@��+@�{@�/@���@�j@��F@�;d@�
=@��@�&�@���@�r�@�Q�@�9X@��@���@�M�@��`@�z�@�r�@�bN@�I�@�1@���@�
=@���@���@�n�@�5?@|�@s"�@g�;@`�`@Z�H@RM�@H��@B��@:��@6$�@.@(b@#@�R@1@@�;@�+@9X1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�K�A�M�A�M�A�O�A�O�A�O�A�O�A�M�A�G�A�K�A�O�A�O�A�Q�A�Q�A�K�A�K�A�S�A�XA�ZA�XA�\)A�^5A�^5A�`BA�`BA�`BA�bNA�dZA�G�Aҧ�A���A�A�A��A���A��HA�VAʍPA�-A�|�A��yA+A��!A��wA��A��A�z�A�ZA���A�%A�33A�~�A�ƨA��7A�M�A���A��^A��A���A���A�l�A�S�A�$�A�%A��A�=qA�x�A�O�A�+A��A��FA�G�A��/A���A�ȴA�?}A���A�ZA�+A�z�A�-A�9XA�ĜA���A��+A�S�A��!A�7LA�1A���A��A���A���A�E�A���A��A�1A�n�A���A��A��A�bA�ffA�&�A�^5A�Q�A���A�
=A�r�A�+A�
=A���A��A��/A���A�bNA��+A��A�K�A�JA��mA�A�  A~�A|5?Ay
=Au�FAt��Aq�Al �Af�+Ac�wAb5?Aa|�A_��A]�hA\AX�9AW33AV�AV~�AR5?AMp�AL��AK�hAKAI�AG�^AF5?AD�`ADI�AC�ACAC�AB1'A@�yA@�A=��A<�+A<1A;C�A8ȴA8JA7�-A7�A6��A6r�A5��A3l�A1O�A/p�A.bNA-��A-A,�+A,�A+x�A)\)A'�A'"�A%�A$�A#��A"��A"�\A"A!��A!&�A �+A =qAA��A��A��Az�AQ�A(�AA�A�jA�A��A|�AM�A��AS�AVA�AXA��A5?A
1A	l�A��A�^A��A�wA�/AJA�;A��A �@�Ĝ@��@�33@�"�@�o@�
=@�@���@��@�  @�/@�ƨ@�Z@��H@�@睲@�M�@�@��@�@�G�@�K�@�$�@���@ۍP@�^5@�O�@���@��@���@��@�j@� �@�^5@ѡ�@�G�@�I�@�ƨ@�"�@�`B@�%@�j@��
@�"�@ʧ�@�~�@�@�hs@�?}@��@�V@���@�b@�l�@�33@�E�@�r�@�C�@�?}@�\)@���@�J@�p�@��@��j@��@���@�Z@�9X@� �@�1@��@��;@��;@��;@��;@��
@���@��@���@���@�"�@�5?@���@�O�@��`@���@��j@���@�z�@�bN@�Q�@�A�@�1@��@�S�@���@�^5@�n�@���@���@���@�r�@��@��@�dZ@�C�@�ȴ@��@�bN@��@���@�|�@�|�@�;d@��@�~�@�E�@�5?@���@���@���@�j@���@���@��;@��;@��w@��@�\)@�
=@��@��@��R@��H@���@�^5@�M�@�5?@�$�@��T@�x�@�`B@�`B@��7@�`B@��`@��@�(�@�ƨ@�t�@�@�ff@��@��@���@��@�z�@�b@���@�t�@�33@��@��H@��R@��+@�n�@�=q@�@���@��-@���@��7@�O�@��@���@�Ĝ@���@�I�@��;@��w@���@��@�\)@�;d@�o@���@���@��y@��@��@�ȴ@���@���@�&�@���@���@��9@���@���@��@�bN@�bN@��@�C�@�v�@�{@���@��T@���@��^@���@���@��7@�x�@�`B@�/@��@�V@��/@�bN@��;@�S�@�~�@�x�@�hs@�`B@�G�@�/@��@��`@���@���@��9@��D@�  @��P@�+@��y@��+@�{@�/@���@�j@��F@�;d@�
=@��@�&�@���@�r�@�Q�@�9X@��@���@�M�@��`@�z�@�r�@�bN@�I�@�1@���@�
=@���@���@�n�@�5?@|�@s"�@g�;@`�`@Z�H@RM�@H��@B��@:��@6$�@.@(b@#@�R@1@@�;@�+@9X1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�9B�9B�9B�9B�9B�9B�9B�9B�9B�9B�9B�9B�9B�9B�9B�?B�9B�9B�9B�9B�9B�9B�9B�9B�3B�3B�3B�'B�!B�'B�wB�qB�XB��B�\B}�Bp�BS�BVBcTBr�B�B�JB��B�B�9B�?B�?B�FB�3B�3B�?B�LB�LB�dBÖBĜBŢBĜBŢBŢBŢBĜBÖBB�}B�wB�jB�^B�LB�3B�3B�!B�B��B��B��B��B�JB�Bv�Bp�BgmBe`BcTB^5BZBXBW
BT�BQ�BK�B?}B+B�B	7B��B��B�B�/B��BB�Bt�BR�BF�B=qB5?B-B#�BVB
��B
��B
�'B
��B
��B
�DB
�B
{�B
m�B
]/B
N�B
5?B
�B
B	�B	�;B	ȴB	��B	� B	jB	_;B	W
B	L�B	?}B	1'B	$�B	�B	�B	PB	B��B��B��B�B�B�`B�TB�;B�)B�B�B�B��B��B��BɺBŢB��B�qB�^B�LB�?B�-B�!B�B��B��B��B�uB�{B�{B�uB�oB�bB�PB�7B�%B�B�B� B~�B�B�B�%B�B�B�B�B�B�B�B�B�B� B}�Bz�Bs�Bs�Bt�Bs�Br�Br�Br�Bs�Bp�Bm�Bl�Bk�BiyBjBffBdZBbNBaHB_;B^5B]/BYBW
BW
BYBZBZBYBYBW
BT�BQ�BN�BR�BS�BR�BW
BW
BW
BXBXBXB[#BaHBaHBdZBl�Br�Bu�Bw�By�B|�B|�B� B�B�B�B�=B�JB�JB�hB�hB�oB��B��B��B��B��B��B��B��B�B�!B�3B�3B�3B�LB�XB�XB�XB�qBĜBƨBƨBƨBȴB��B��B��B�
B�#B�;B�BB�HB�HB�NB�NB�TB�TB�TB�ZB�`B�fB�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B	  B	B	JB	bB	uB	�B	�B	�B	 �B	#�B	&�B	'�B	'�B	(�B	(�B	)�B	+B	+B	,B	-B	.B	.B	/B	0!B	33B	49B	6FB	:^B	>wB	B�B	D�B	D�B	D�B	D�B	F�B	R�B	S�B	T�B	XB	ZB	]/B	^5B	_;B	_;B	`BB	aHB	bNB	cTB	e`B	gmB	iyB	iyB	l�B	n�B	p�B	r�B	s�B	s�B	s�B	s�B	t�B	u�B	x�B	y�B	|�B	~�B	�B	�B	�B	�B	�B	�%B	�1B	�=B	�DB	�DB	�DB	�PB	�bB	�hB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�!B	�'B	�-B	�3B	�9B	�?B	�RB	�dB	�jB	�jB	�jB	�qB	�qB	�qB	�qB	�qB	�qB	�qB	�wB	�wB	�wB	�wB	��B	��B	�}B	�wB	B	B	B	ÖB	ĜB	ĜB	ĜB	ĜB	ĜB	ĜB	ĜB	ŢB	ǮB	ȴB	ɺB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�B	�#B	�)B	�HB	�NB	�NB	�TB	�NB	�TB	�ZB	�fB	�fB	�mB	�mB	�B	��B
B
VB
�B
#�B
-B
6FB
=qB
D�B
I�B
O�B
VB
\)B
aHB
cTB
jB
p�B
r�B
u�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�9B�9B�9B�9B�9B�9B�9B�9B�9B�9B�9B�9B�9B�9B�9B�?B�9B�9B�9B�9B�9B�9B�9B�9B�3B�3B�3B�3B�LB�wBÖBBĜB�dB��B�+B�%BiyBdZBp�B�B�VB��B�FB�wB�wB�^B�}B�wB�dB�^B�RB�dB�wBŢB��BǮBǮBƨBƨBǮBǮBȴB��BȴB��B��B�}B�wB�wBÖB��B�XB�3B�B�B�B��B��B�DB}�By�BjBiyBjBcTB\)BYBXBXBZB]/BQ�B>wB!�BbBB��B��B�fB�HB�/B�;B�=B\)BM�BD�B:^B7LB@�B>wB
�;B
��B
�^B
�-B
��B
�hB
�7B
�DB
z�B
n�B
e`B
G�B
2-B
�B	��B	��B	�B	ÖB	�\B	s�B	gmB	bNB	\)B	L�B	D�B	-B	 �B	!�B	+B	�B	B	  B��B��B��B�B�B�TB�;B�/B�5B�5B�5B�#B�;B��B��B��B��B�}B�^B�XB�?B�?B�FB�^B�B��B��B��B��B��B��B��B��B�{B�JB�DB�=B�%B�B�%B�7B�=B�7B�1B�1B�7B�=B�7B�7B�B�B�B�B�B�B� Bz�By�B{�B|�B{�Bv�Bu�Bw�Bx�Bv�Bu�Bo�Bl�Bl�BjBgmBffBcTBaHBcTBe`BcTB\)B[#BZBYBYBXBYB[#B^5B\)B[#B]/B]/B^5B[#B]/B^5B]/B_;BdZBgmBhsBp�Bw�By�B{�B~�B� B�B�B�B�+B�7B�PB�VB�bB�hB�{B��B��B��B��B��B��B��B��B��B�B�'B�3B�?B�FB�XB�dB�wB�}BÖB��B��BȴBɺBȴB��B��B��B�
B�#B�;B�BB�HB�HB�NB�NB�TB�TB�TB�ZB�`B�fB�B�B�B��B��B��B��B��B��B��B��B��B	  B	  B��B��B��B	  B	B	JB	bB	�B	�B	�B	�B	 �B	#�B	(�B	'�B	-B	)�B	)�B	)�B	+B	,B	-B	.B	/B	/B	1'B	33B	33B	5?B	7LB	:^B	>wB	B�B	E�B	E�B	E�B	D�B	F�B	S�B	S�B	T�B	YB	[#B	]/B	^5B	_;B	`BB	aHB	aHB	bNB	cTB	ffB	hsB	jB	jB	m�B	o�B	q�B	t�B	t�B	s�B	t�B	u�B	u�B	u�B	y�B	y�B	}�B	� B	�B	�B	�B	�B	�%B	�%B	�1B	�=B	�DB	�DB	�JB	�VB	�bB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�!B	�'B	�-B	�3B	�?B	�LB	�^B	�jB	�jB	�jB	�jB	�qB	�qB	�qB	�qB	�qB	�qB	�qB	�wB	�wB	�}B	�}B	��B	B	��B	��B	B	B	B	ÖB	ĜB	ĜB	ĜB	ĜB	ĜB	ĜB	ŢB	ƨB	ȴB	ɺB	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�)B	�5B	�NB	�NB	�NB	�TB	�NB	�ZB	�`B	�fB	�fB	�mB	�mB	�B	��B
B
VB
�B
"�B
-B
6FB
=qB
D�B
I�B
O�B
VB
\)B
aHB
cTB
jB
p�B
r�B
u�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<D��<�C�<T��<#�
<�1<�1<e`B<T��<�o<49X<�C�<�C�<�t�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�o<T��<#�
<#�
<#�
<#�
<T��<#�
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
<�C�<�t�<���<49X<#�
<#�
<#�
<D��<#�
<e`B<���=P�`<�1<#�
<#�
<#�
<#�
<#�
<�`B=@�<T��<#�
<#�
<49X<#�
<#�
<#�
<u<T��<�C�<�9X<�t�<�9X<��
<T��<���=t�=o<�o<#�
<#�
<D��<u<T��<���<#�
<#�
<49X<�h<ě�<#�
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
<u<#�
<#�
<#�
<e`B<#�
<#�
<#�
<#�
<#�
<#�
<�t�<u<49X<#�
<#�
<#�
<#�
<#�
<#�
<u<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�C�<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<49X<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B<D��<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.2 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447342012010314473420120103144734  AO  ARGQ                                                                        20111130143031  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143031  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144734  IP                  G�O�G�O�G�O�                