CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:31Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         ZPRIMARY | https://orcid.org/0000-0001-7324-3159 | Matthew Alkire, University of Washington        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7(   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7,   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    70   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7@   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7P   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7`   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7h   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8    DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8$   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8D   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8H   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8L   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8l   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �T   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �d   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �h   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �x   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �|   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170931  20220204114426  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @��N ��1   @����`�@5��l�C��b�Z�11   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�  @�  A   A   A@  A`  A�  A�33A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B?��BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C�C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CU�fCX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7fD7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DLy�DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt� Dy�)D� �D�P�D�� D��{D��D�S�D�� D�D�fD�W�D��RD���D�\D�X�Dږ�D��D��D�T{D��D�s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@���@���AffA:ffAZffAzffA�ffA�33A�33A�33A�33A�33A�33A�33B��B��B��B��B&��B.��B6��B>34BF��BN��BV��B^��Bf��Bn��Bv��B~��B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�C�fC�fC�fC�fC	�fC�fC� C�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU��CW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�� C�� C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D i�D �Di�D�Di�D�Dp D�Di�D�Di�D�Di�D�Di�D�Di�D�D	i�D	�D
i�D
�Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�D i�D �D!i�D!�D"i�D"�D#i�D#�D$i�D$�D%i�D%�D&i�D&�D'i�D'�D(i�D(�D)i�D)�D*i�D*�D+i�D+�D,i�D,�D-i�D-�D.i�D.�D/i�D/�D0i�D0�D1i�D1�D2i�D2�D3i�D3�D4i�D4�D5i�D5�D6i�D6� D7i�D7�D8i�D8�D9i�D9�D:i�D:�D;i�D;�D<i�D<�D=i�D=�D>i�D>�D?i�D?�D@i�D@�DAi�DA�DBi�DB�DCi�DC�DDi�DD�DEi�DE�DFi�DF�DGi�DG�DHi�DH�DIi�DI�DJi�DJ�DKi�DK�DLc4DL�DMi�DM�DNi�DN�DOi�DO�DPi�DP�DQi�DQ�DRi�DR�DSi�DS�DTi�DT�DUi�DU�DVi�DV�DWi�DW�DXi�DX�DYi�DY�DZi�DZ�D[i�D[�D\i�D\�D]i�D]�D^i�D^�D_i�D_�D`i�D`�Dai�Da�Dbi�Db�Dci�Dc�Ddi�Dd�Dei�De�Dfi�Df�Dgi�Dg�Dhi�Dh�Dii�Di�Dji�Dj�Dki�Dk�Dli�Dl�Dmi�Dm�Dni�Dn�Doi�Do�Dpi�Dp�Dqi�Dq�Dri�Dr�Dsi�Ds�Dti�DtɚDy��D��D�EqD�|�D��HD��D�H�D���D��\D�3D�L{D��D�ιD�)D�MqDڋ�D��RD�{D�IHD�D�hR111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�7LA�1'A�5?A�33A�=qA�?}A�?}A���A�XA���Aİ!A�|�A�n�A�^5A�C�A��AÏ\A��A��A��DA�ZA��hA���A���A��A���A���A�ffA�-A�ƨA��A�n�A�
=A��^A�O�A��
A�&�A��/A��/A��#A��^A�v�A�\)A�=qA�
=A��#A���A��A�;dA�+A��HA���A�O�A��A��^A��hA�~�A�hsA�O�A��A���A�ȴA���A�ffA��A���A�ZA��A���A�I�A�
=A���A��A�x�A��wA�ĜA�+A�~�A���A� �A��A���A��TA�A�~�A�`BA��
A�XA� �A�ƨA��
A��-A��DA�dZA��\A��A�ZA���A�ƨA���A��hA��!A��A���A��#A�5?A���A�/A�oA��
A�33A�x�A�jA� �A��DA�-A�/A�bNA���A�`BA���A�jA��A�33A�x�A���A�A��A~��Ax��Aup�At��As�Ap�RAn��Amt�AlffAil�Ag��Ab�/A_|�A^ĜA\5?AY�7AVn�AR9XAO�AN$�AL�AJ��AI7LAF^5ADffAB��AA��A?�wA=�A<�!A:v�A9G�A9%A8�yA8��A8I�A7
=A533A2�`A2Q�A1|�A/��A/+A-�A,�`A,I�A+33A*M�A(z�A(bNA'��A%��A$~�A#��A"�A!dZA �`Ap�A��A��AQ�A �A�^A�AZA/A�;A��A(�A33AZA�PA��A(�AA�HA��A��A  A��A�A��A1'A
I�A	hsA~�A��A\)A��AO�A=qA�A�A��AhsA�A ��@�l�@��w@�X@���@��@��@�@��@�K�@��^@�S�@�  @��;@�|�@�\@�`B@��/@䛦@� �@�ƨ@�l�@�J@���@��@�Q�@١�@�A�@�+@�/@ԓu@�A�@��@��m@�K�@�&�@�j@϶F@�ȴ@�@�V@̋D@�;d@�=q@���@�X@�1'@Ǖ�@�;d@��#@�Ĝ@�bN@��
@�C�@�@��u@���@��F@��P@�o@��\@�?}@��D@���@���@�=q@�O�@�j@���@�|�@��@��@�X@�%@���@�1'@�;d@�n�@��@���@�p�@�/@���@��
@�K�@���@�v�@��@�G�@��@�1@���@�l�@�S�@�@��#@��u@���@���@��
@��F@��+@�V@���@�~�@��\@�~�@�^5@��@��#@��h@��@���@�Z@�(�@��;@�|�@�+@�
=@�ȴ@��@�G�@�%@��9@�Z@��@��m@�\)@�33@��@���@�ȴ@�n�@�-@��@��^@��@�7L@���@���@�Q�@�(�@��;@�|�@�S�@�
=@���@�M�@��@��T@���@�x�@�/@��`@�Ĝ@��@�A�@��m@�|�@�C�@��@���@�n�@�=q@���@���@���@���@�X@�X@�?}@���@��@�bN@�I�@�1@�ƨ@�t�@�33@�v�@�-@���@�&�@�G�@��@���@��u@� �@�33@��@���@�
=@�l�@��@��+@�@���@��#@���@��@�`B@�%@��/@�Ĝ@�V@��/@���@�j@�(�@��@���@�C�@�@���@�ȴ@���@���@�n�@�{@��#@��^@���@�hs@��/@��9@���@�9X@���@��w@���@��P@�|�@�dZ@�K�@�
=@��@���@���@��+@�n�@�^5@�=q@�@��-@��^@���@���@�hs@�&�@��u@�z�@�j@�9X@�  @�33@�"�@�C�@� �@���@�;d@��y@���@�v�@�^5@�E�@�I�@ye,@q?}@k1�@dU2@Y�@S�Q@M��@G��@@D�@:.�@4�e@0�4@*��@%�C@ w�@�+@
=@��@7�@u�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�7LA�1'A�5?A�33A�=qA�?}A�?}A���A�XA���Aİ!A�|�A�n�A�^5A�C�A��AÏ\A��A��A��DA�ZA��hA���A���A��A���A���A�ffA�-A�ƨA��A�n�A�
=A��^A�O�A��
A�&�A��/A��/A��#A��^A�v�A�\)A�=qA�
=A��#A���A��A�;dA�+A��HA���A�O�A��A��^A��hA�~�A�hsA�O�A��A���A�ȴA���A�ffA��A���A�ZA��A���A�I�A�
=A���A��A�x�A��wA�ĜA�+A�~�A���A� �A��A���A��TA�A�~�A�`BA��
A�XA� �A�ƨA��
A��-A��DA�dZA��\A��A�ZA���A�ƨA���A��hA��!A��A���A��#A�5?A���A�/A�oA��
A�33A�x�A�jA� �A��DA�-A�/A�bNA���A�`BA���A�jA��A�33A�x�A���A�A��A~��Ax��Aup�At��As�Ap�RAn��Amt�AlffAil�Ag��Ab�/A_|�A^ĜA\5?AY�7AVn�AR9XAO�AN$�AL�AJ��AI7LAF^5ADffAB��AA��A?�wA=�A<�!A:v�A9G�A9%A8�yA8��A8I�A7
=A533A2�`A2Q�A1|�A/��A/+A-�A,�`A,I�A+33A*M�A(z�A(bNA'��A%��A$~�A#��A"�A!dZA �`Ap�A��A��AQ�A �A�^A�AZA/A�;A��A(�A33AZA�PA��A(�AA�HA��A��A  A��A�A��A1'A
I�A	hsA~�A��A\)A��AO�A=qA�A�A��AhsA�A ��@�l�@��w@�X@���@��@��@�@��@�K�@��^@�S�@�  @��;@�|�@�\@�`B@��/@䛦@� �@�ƨ@�l�@�J@���@��@�Q�@١�@�A�@�+@�/@ԓu@�A�@��@��m@�K�@�&�@�j@϶F@�ȴ@�@�V@̋D@�;d@�=q@���@�X@�1'@Ǖ�@�;d@��#@�Ĝ@�bN@��
@�C�@�@��u@���@��F@��P@�o@��\@�?}@��D@���@���@�=q@�O�@�j@���@�|�@��@��@�X@�%@���@�1'@�;d@�n�@��@���@�p�@�/@���@��
@�K�@���@�v�@��@�G�@��@�1@���@�l�@�S�@�@��#@��u@���@���@��
@��F@��+@�V@���@�~�@��\@�~�@�^5@��@��#@��h@��@���@�Z@�(�@��;@�|�@�+@�
=@�ȴ@��@�G�@�%@��9@�Z@��@��m@�\)@�33@��@���@�ȴ@�n�@�-@��@��^@��@�7L@���@���@�Q�@�(�@��;@�|�@�S�@�
=@���@�M�@��@��T@���@�x�@�/@��`@�Ĝ@��@�A�@��m@�|�@�C�@��@���@�n�@�=q@���@���@���@���@�X@�X@�?}@���@��@�bN@�I�@�1@�ƨ@�t�@�33@�v�@�-@���@�&�@�G�@��@���@��u@� �@�33@��@���@�
=@�l�@��@��+@�@���@��#@���@��@�`B@�%@��/@�Ĝ@�V@��/@���@�j@�(�@��@���@�C�@�@���@�ȴ@���@���@�n�@�{@��#@��^@���@�hs@��/@��9@���@�9X@���@��w@���@��P@�|�@�dZ@�K�@�
=@��@���@���@��+@�n�@�^5@�=q@�@��-@��^@���@���@�hs@�&�@��u@�z�@�j@�9X@�  @�33@�"�@�C�@� �@���@�;d@��y@���@�v�@�^5G�O�@�I�@ye,@q?}@k1�@dU2@Y�@S�Q@M��@G��@@D�@:.�@4�e@0�4@*��@%�C@ w�@�+@
=@��@7�@u�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	��B	��B	��B	��B	��B	��B	��B	�B
B
)�B
.B
1'B
33B
33B
49B
5?B
8RB
8RB
6FB
6FB
49B
.B
&�B
#�B
!�B
 �B
�B
�B
�B
 �B
"�B
#�B
)�B
.B
2-B
,B
/B
=qB
B�B
C�B
K�B
VB
YB
_;B
iyB
q�B
x�B
� B
�hB
�uB
��B
�B
�dB
ȴB
��B
�5B
�ZB
�B
��BJB�B'�B>wBJ�BYBaHBw�B�7B��B��B��B�'B�?B�jB�
B�B��BBDBbBbBbBPBPBoB�B�B"�B�B�BVBVB�B'�B�BVBB��B��B�B�B�B�TB�BǮB�XB�B��B�%B�Bx�Bp�B`BBZBR�B=qB�BbB
��B
�B
�HB
��B
�RB
��B
�JB
r�B
S�B
?}B
1'B
bB	�B	�ZB	�B	ǮB	�?B	�B	��B	�VB	}�B	ffB	N�B	F�B	8RB	�B	B�B�yB�`B�HB�B��BȴBBB�qB�!B��B��B��B��B��B��B��B��B�oB�DB�B}�B~�By�Bx�Bt�Br�Bq�Bm�Bk�BhsBhsBgmB_;B\)BXBW
BXBXBYBYB^5BffBhsBhsBgmBgmBffBcTBbNBaHBaHB`BB`BB`BBgmBjBiyBiyBn�Bm�Bm�Bl�Bk�Bk�BiyBffBe`BcTBaHBaHB`BB_;B\)B\)B\)B]/B^5BbNBaHB`BBYBXBVBS�BS�BP�BN�BJ�BF�B<jB;dB<jB>wB@�B@�BA�BE�BD�BD�BC�BA�B9XB49B2-B0!B1'B33B5?B6FB6FB6FB7LB=qB=qB?}B@�BB�BC�BC�BF�BF�BG�BG�BM�BM�BM�BS�BW
BXBYBZB^5B`BBbNBbNBbNBdZBdZBhsBk�Bo�Br�Bt�Bw�By�B{�B}�B�B�B�B�B�B�+B�1B�JB�JB�PB�PB�\B�uB��B��B��B��B��B��B�B�-B�^B�qB�}B�}B�qB�qB�wB��BĜBɺB��B��B��B�B�#B�/B�5B�;B�TB�`B�sB�B�B�B�B��B��B��B��B��B	B	B	+B	DB	VB	\B	oB	{B	�B	�B	�B	!�B	#�B	&�B	'�B	)�B	+B	-B	1'B	49B	7LB	:^B	?}B	A�B	C�B	H�B	N�B	P�B	Q�B	VB	XB	\)B	`BB	bNB	bNB	ffB	iyB	m�B	p�B	q�B	s�B	y�B	|�B	~�B	�B	�B	�B	�B	�7B	�=B	�\B	�hB	�oB	�oB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�3B	�9B	�LB	�XB	�^B	�dB	�wB	�wB	�qB	�qB	�}B	��B	B	B	B	ĜB	ĜB	ÖB	ÖB	ĜB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�#B	�/B	�;B	�;B	�HB	�HB	�ZB	�ZB	�`B	�`B	�`B	�`B	�`B	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B
�B
?B
!|B
%�B
1vB
<B
B�B
HfB
O�B
S�B
YB
\]B
_�B
ezB
jKB
n/B
s�B
u�B
z*B
{d111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B	�CB	�CB	�CB	�>B	�>B	�>B	�8B	��B	�B
rB
�B
!�B
#�B
#�B
$�B
%�B
(�B
(�B
&�B
&�B
$�B
�B
bB
QB
EB
?B
8B
3B
9B
@B
KB
QB
vB
�B
"�B
�B
�B
-�B
3B
4B
<=B
FyB
I�B
O�B
Y�B
bB
iGB
pqB
��B
��B
�2B
�tB
��B
�B
�MB
ΛB
��B
��B
�>B
��B�BNB.�B;BIoBQ�Bh$By�B��B�B�FB�vB��B��B�UB��B�.B�eB��B �B �B �B��B��B�B�B�BBB�B��B��B�B6B�B��B�^B�AB�B�B��B��BӥB�cB�B��B�tB�Bv�BsqBi6BaBP�BJ�BCXB-�B,B �B
�HB
�B
ѾB
�XB
��B
�nB
|�B
c6B
D�B
0
B
!�B
 �B	�B	��B	ʹB	�MB	��B	��B	�uB	~�B	n�B	WB	?�B	7ZB	)B	jB��B�]B�9B�!B�
B��B��B�zB�VB�VB�9B��B��B��B��B��B�zB�zB�oB�bB�?B|Br�Bn�Bo�Bj�Bi�Be�Bc�Bb�B^hB\\BYKBYKBXFBPBMBH�BG�BH�BH�BI�BI�BOBWBBYNBYNBXIBXIBWBBT1BS+BR&BR&BQ BQ!BQ!BXKB[]BZWBZWB_vB^oB^pB]jB\dB\dBZYBWFBVABT5BR*BR*BQ$BPBMBMBMBNBOBS1BR,BQ&BI�BH�BF�BD�BD�BA�B?�B;�B7�B-UB,OB-UB/bB1nB1nB2tB6�B5�B5�B4�B2tB*EB%'B#B!B"B$"B&.B'4B'5B'5B(:B._B._B0kB1qB3}B4�B4�B7�B7�B8�B8�B>�B>�B>�BD�BG�BH�BJBK
BO!BQ.BS:BS:BS:BUFBUFBY_B\qB`�Bc�Be�Bh�Bj�Bl�Bn�Bq�Bs�Bv	Bv	Bv	BxByB}3B}3B~9B~9B�EB�^B�oB�{B��B��B��B��B��B�B�CB�VB�aB�aB�VB�VB�\B�hB��B��B��B��B��B��B�B�B�B�B�5B�AB�SB�qB�~B�B�B�B�B�B��B��B��B��B�B�!B�3B	 9B	KB	WB	cB	
vB	�B	�B	�B	�B	�B	�B	�B	�B	" B	%B	(%B	+6B	0UB	2`B	4mB	9�B	?�B	A�B	B�B	F�B	H�B	L�B	QB	S"B	S"B	W:B	ZLB	^dB	awB	b|B	d�B	j�B	m�B	o�B	r�B	s�B	t�B	u�B	zB	{B	�+B	�7B	�>B	�>B	�>B	�DB	�JB	�nB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�)B	�/B	�BB	�BB	�<B	�<B	�HB	�TB	�YB	�YB	�ZB	�fB	�fB	�aB	�aB	�fB	�rB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ûB	ûB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�!B	�'B	�'B	�'B	�'B	�'B	�4B	�:B	�FB	�FB	�LB	�RB	�vB	�|B	�|B	�vB	�vB	�|B	�|G�O�B	�B
 nB
B
>B
�B
"7B
,�B
3�B
9&B
@�B
D�B
J>B
MB
PcB
V8B
[	B
^�B
dYB
feB
j�B
l!111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.35 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9996(+/-0.0001), vertically averaged dS =-0.015(+/-0.005) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144262022020411442620220204114426  AO  ARCAADJP                                                                    20200619170931    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170931  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170931  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114426  IP                  G�O�G�O�G�O�                