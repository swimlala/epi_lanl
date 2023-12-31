CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-18T14:13:36Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200618141336  20220204114410  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @؂[�ٿ�1   @؂\fftp@6���S���c�V�u1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  BffB ��B'��B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*�C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D y�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fDfD� D  D� D  D� D  Dy�D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&y�D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-fD-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DUy�DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ�fD[fD[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3DyD��D�]�D���D��RD� D�j�D��\D��{D��{D�Y�D���D��D�)�D�c�DڞD��\D�D�^�D�D�Ӆ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @|��@���@���AffA:ffAZffAzffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B  BfgB&34B.��B6��B>��BF��BN��BV��B^��Bf��Bn��Bv��B~��B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B��B�L�B�L�B�L�B�L�B�L�B�L�B�L�C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)� C+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D c4D �Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�D	i�D	�D
i�D
�Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�Di�D�Dp D� Di�D�Di�D�Di�D�Dc4D�Di�D�Di�D�Di�D�D i�D �D!i�D!�D"i�D"�D#i�D#�D$i�D$�D%i�D%�D&c4D&�D'i�D'�D(i�D(�D)i�D)�D*i�D*�D+i�D+�D,i�D,� D-i�D-�D.i�D.�D/i�D/�D0i�D0�D1i�D1�D2i�D2�D3i�D3�D4i�D4�D5i�D5�D6i�D6�D7i�D7�D8i�D8�D9i�D9�D:i�D:�D;i�D;�D<i�D<�D=i�D=�D>i�D>�D?i�D?�D@i�D@�DAi�DA�DBi�DB�DCi�DC�DDi�DD�DEi�DE�DFi�DF�DGi�DG�DHi�DH�DIi�DI�DJi�DJ�DKi�DK�DLi�DL�DMi�DM�DNi�DN�DOi�DO�DPi�DP�DQi�DQ�DRi�DR�DSi�DS�DTi�DT�DUc4DU�DVi�DV�DWi�DW�DXi�DX�DYi�DY�DZp DZ� D[i�D[�D\i�D\�D]i�D]�D^i�D^�D_i�D_�D`i�D`�Dai�Da�Dbi�Db�Dci�Dc�Ddi�Dd�Dei�De�Dfi�Df�Dgi�Dg�Dhi�Dh�Dii�Di�Dji�Dj�Dki�Dk�Dli�Dl�Dmi�Dm�Dni�Dn�Doi�Do�Dpi�Dp�Dqi�Dq�Dri�Dr�Dsi�Ds�Dti�Dt��Dy�)D� �D�R�D���D��D��D�_\D��)D��HD��HD�N�D���D���D�gD�X�Dڒ�D��)D��D�S�D�RD��R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AӬAӬAӮAӮAӬAө�Aӡ�Aӥ�Aӥ�A�XA�M�A�I�A�9XA�33A�&�A� �A���A�$�A�\)A���A�VA�
=A�l�A��AĲ-A�x�A��A��A��wA��7A��\A��TA�-A�dZA���A�+A��jA��9A�1'A�9XA�?}A�A��A��DA���A�?}A���A�-A�1A���A���A��A�^5A���A�A���A�t�A�A�A�1'A��A�bA���A���A��A�dZA�$�A�O�A���A��jA��A��A��A��A��+A�M�A��/A��\A�|�A�p�A�M�A���A�O�A���A��A�  A���A�A���A��A���A�ĜA�O�A�dZA��/A��yA���A��A��hA��A�(�A�$�A��jA�/A��yA�9XA~�RA}G�A|��A|JAz��AyO�AvI�At�`As��Ar�ApJAo%AmAk��Ajv�AiƨAi&�Ah��Ag�
Af�Ad��Ac��Ab�HAaS�A`(�A_x�A]7LA\ZA[K�AZ�+AY%AU�AUK�AT��AS��AR�jAQ\)AN�uAL��AK`BAJn�AIoAG
=AE�FADA�AC�7AB�/AA��A@�jA@=qA?O�A>bA=��A=
=A<A�A;\)A:Q�A9��A9C�A8��A8r�A8JA7x�A6�A6 �A5�A4�uA3�^A3"�A2�RA1;dA/x�A/A.A�A-��A,=qA*�`A*n�A*�A)t�A(��A'��A'33A&��A%VA#��A!dZAt�A�A�HA�!A-AG�A  A��A33A��A�;A�9A�AȴA��AffA?}A~�Ax�A�yA��At�A;dA
jA	K�AoA��A��A�A�AA �@��
@�+@��y@�-@��@���@�v�@�V@�5?@��@���@�%@���@��@��9@��@���@��@��@���@�@�?}@�I�@�\)@�ȴ@�E�@��@��`@�~�@�F@��@�Ĝ@�  @�|�@�hs@۶F@�K�@�"�@�@�ȴ@؛�@���@�-@���@�V@��@д9@�1'@�t�@·+@���@�  @�V@�&�@ȓu@��;@ǥ�@ǥ�@ǝ�@�t�@�"�@��@�1'@���@öF@��y@���@���@�1'@�t�@��@�=q@��@�  @��F@�;d@�v�@�@���@�1'@���@��-@�V@��@�o@�J@�O�@�V@���@�Ĝ@��m@��R@�X@�V@�bN@�l�@���@�5?@���@�x�@�7L@���@���@��@���@�"�@��H@�~�@�@��h@�O�@�/@��@�%@��/@��j@� �@��@�~�@��@�-@�$�@��^@�hs@��`@��@�Z@��@�~�@��-@��@��j@��@��@���@��@�~�@��#@���@�`B@�G�@�9X@�@��@�/@��#@��@��^@�`B@�hs@�p�@�x�@�x�@���@�1'@�1'@�  @��@�~�@�%@�%@��`@�Ĝ@���@��@�X@�`B@�V@��/@��@���@��@��F@�"�@���@�ff@�M�@�n�@�o@�;d@�+@��@���@���@��\@�n�@�{@���@�G�@�(�@��
@��@�@���@���@�O�@��@�b@��w@�dZ@�K�@�+@��H@��+@���@��\@��\@��!@�v�@�E�@���@���@��@�M�@�V@�E�@�$�@���@��^@�x�@�?}@��@�p�@�X@�G�@�/@���@�Ĝ@���@��9@���@���@��@�Z@�9X@��@�  @��@��;@��w@�|�@�
=@��@��@���@��h@�`B@��@��/@��u@�j@�bN@�Q�@� �@��;@��
@��@�|�@���@�ȴ@��+@�{@��@��-@��@�7�@x?�@p$@g�r@`u�@X��@P-�@H��@B҉@=��@4�@/�*@*W�@#�:@ ��@�/@�5@@1'@N�@�f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  AӬAӬAӮAӮAӬAө�Aӡ�Aӥ�Aӥ�A�XA�M�A�I�A�9XA�33A�&�A� �A���A�$�A�\)A���A�VA�
=A�l�A��AĲ-A�x�A��A��A��wA��7A��\A��TA�-A�dZA���A�+A��jA��9A�1'A�9XA�?}A�A��A��DA���A�?}A���A�-A�1A���A���A��A�^5A���A�A���A�t�A�A�A�1'A��A�bA���A���A��A�dZA�$�A�O�A���A��jA��A��A��A��A��+A�M�A��/A��\A�|�A�p�A�M�A���A�O�A���A��A�  A���A�A���A��A���A�ĜA�O�A�dZA��/A��yA���A��A��hA��A�(�A�$�A��jA�/A��yA�9XA~�RA}G�A|��A|JAz��AyO�AvI�At�`As��Ar�ApJAo%AmAk��Ajv�AiƨAi&�Ah��Ag�
Af�Ad��Ac��Ab�HAaS�A`(�A_x�A]7LA\ZA[K�AZ�+AY%AU�AUK�AT��AS��AR�jAQ\)AN�uAL��AK`BAJn�AIoAG
=AE�FADA�AC�7AB�/AA��A@�jA@=qA?O�A>bA=��A=
=A<A�A;\)A:Q�A9��A9C�A8��A8r�A8JA7x�A6�A6 �A5�A4�uA3�^A3"�A2�RA1;dA/x�A/A.A�A-��A,=qA*�`A*n�A*�A)t�A(��A'��A'33A&��A%VA#��A!dZAt�A�A�HA�!A-AG�A  A��A33A��A�;A�9A�AȴA��AffA?}A~�Ax�A�yA��At�A;dA
jA	K�AoA��A��A�A�AA �@��
@�+@��y@�-@��@���@�v�@�V@�5?@��@���@�%@���@��@��9@��@���@��@��@���@�@�?}@�I�@�\)@�ȴ@�E�@��@��`@�~�@�F@��@�Ĝ@�  @�|�@�hs@۶F@�K�@�"�@�@�ȴ@؛�@���@�-@���@�V@��@д9@�1'@�t�@·+@���@�  @�V@�&�@ȓu@��;@ǥ�@ǥ�@ǝ�@�t�@�"�@��@�1'@���@öF@��y@���@���@�1'@�t�@��@�=q@��@�  @��F@�;d@�v�@�@���@�1'@���@��-@�V@��@�o@�J@�O�@�V@���@�Ĝ@��m@��R@�X@�V@�bN@�l�@���@�5?@���@�x�@�7L@���@���@��@���@�"�@��H@�~�@�@��h@�O�@�/@��@�%@��/@��j@� �@��@�~�@��@�-@�$�@��^@�hs@��`@��@�Z@��@�~�@��-@��@��j@��@��@���@��@�~�@��#@���@�`B@�G�@�9X@�@��@�/@��#@��@��^@�`B@�hs@�p�@�x�@�x�@���@�1'@�1'@�  @��@�~�@�%@�%@��`@�Ĝ@���@��@�X@�`B@�V@��/@��@���@��@��F@�"�@���@�ff@�M�@�n�@�o@�;d@�+@��@���@���@��\@�n�@�{@���@�G�@�(�@��
@��@�@���@���@�O�@��@�b@��w@�dZ@�K�@�+@��H@��+@���@��\@��\@��!@�v�@�E�@���@���@��@�M�@�V@�E�@�$�@���@��^@�x�@�?}@��@�p�@�X@�G�@�/@���@�Ĝ@���@��9@���@���@��@�Z@�9X@��@�  @��@��;@��w@�|�@�
=@��@��@���@��h@�`B@��@��/@��u@�j@�bN@�Q�@� �@��;@��
@��@�|�@���@�ȴ@��+@�{@��@��-G�O�@�7�@x?�@p$@g�r@`u�@X��@P-�@H��@B҉@=��@4�@/�*@*W�@#�:@ ��@�/@�5@@1'@N�@�f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB0!B1'B1'B1'B1'B1'B33B33B5?B8RB7LB7LB7LB7LB7LB7LB<jBgmBs�B��B�XB��B��B��B�
B�HB��B  BB�B�B �B.BF�BR�Bv�B�RBƨBɺBŢBɺBÖBB�jB�RB�3B�9B�-B�-B�B��B��B��B��B��B�uB�oB�hB�bB�bB�\B�PB�1Bz�Bn�Bl�Be`B]/BW
BP�B=qB49B)�B!�B�B�B�B�B{BoBVB1B�yB�/B��B��B��Bt�BhsBZBH�B;dB0!B(�B�BDB
��B
��B
�B
�BB
��B
�}B
��B
�{B
�7B
}�B
r�B
m�B
iyB
aHB
XB
D�B
8RB
2-B
'�B
�B
hB
DB	��B	��B	�B	�B	�yB	�`B	�;B	�B	��B	��B	ÖB	�jB	�LB	�B	��B	��B	��B	�oB	�B	x�B	v�B	p�B	jB	_;B	Q�B	=qB	2-B	+B	!�B	�B	bB	PB		7B	
=B	1B	B	B	  B��B��B��B��B�B�B�B�B�yB�mB�fB�ZB�NB�;B�)B�B��B��B��B��B��B�}B�qB�^B�RB�-B�FB�?B�9B�3B�B�B�B��B��B��B�uB�hB�bB�\B�PB�DB�B�B�B� B}�B{�Br�Bn�Bk�BffBdZB^5BYBW
BVBS�BR�BP�BM�BI�BF�BH�BC�BC�BB�B>wB<jB;dB:^B9XB8RB7LB6FB6FB6FB5?B5?B5?B33B1'B2-B<jB<jB<jB=qB>wB=qB<jB;dB:^B:^B9XB8RB6FB2-B2-B/B.B-B-B0!B2-B8RB<jB?}B@�BA�B@�BA�BA�BD�BE�BF�BG�BI�BK�BK�BJ�BL�BL�BQ�BT�BVBW
BW
BW
BXBZB^5B`BB`BBdZBiyBjBk�Bl�Bk�Bm�Bo�Bo�Bo�Bo�Bp�Bq�Br�Bs�Bs�Bt�Bt�Bu�By�By�B{�B{�B{�B{�B~�B�B�%B�%B�7B�PB�bB�hB�{B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�'B�3B�LB�RB�XB�}BĜBǮB��B��B��B��B�B�B�ZB�sB�B�B�B��B��B��B��B��B��B��B	  B	B	B	DB	�B	"�B	%�B	'�B	(�B	.B	/B	/B	0!B	1'B	2-B	2-B	33B	49B	8RB	;dB	>wB	?}B	@�B	@�B	C�B	F�B	H�B	K�B	L�B	R�B	S�B	YB	XB	YB	\)B	_;B	cTB	e`B	k�B	n�B	p�B	s�B	u�B	v�B	v�B	w�B	x�B	z�B	{�B	}�B	�B	�B	�B	�B	� B	� B	� B	�B	�1B	�1B	�7B	�=B	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�9B	�?B	�XB	�dB	�}B	��B	��B	��B	��B	ĜB	ǮB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�)B	�/B	�5B	�;B	�BB	�HB	�NB	�TB	�ZB	�`B	�`B	�fB	�yB	�yB	�B	�B	�B	�B	�B	��B	�qB
�B
�B
eB
#TB
.�B
6�B
:�B
B[B
I�B
OBB
T�B
Z�B
]IB
`�B
fLB
h
B
p�B
uB
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B'B(B(B(B(B(B*B*B,&B/9B.3B.3B.3B.3B.3B.3B3PB^QBj�B��B�8BìB��B��B��B�&B�B��B��BuB�B�B$�B=�BI�Bm�B�#B�yB��B�sB��B�hB�bB�=B�&B�B�B�B�B��B��B��B�wB�kB�YB�MB�GB�AB�;B�;B�5B�)BBq�BetBcgB\=BTBM�BG�B4RB+B �B�B�B�BlBfB`B	TB;B�B�aB�B��B�oB��Bk�B_fBQB?�B2[B'B�B�B@B
��B
��B
�B
�BB
��B
�B
��B
��B
�>B
t�B
i�B
d�B
`�B
XSB
OB
;�B
/`B
)<B
 B
�B
zB
WB	�B	��B	�B	�B	��B	�vB	�RB	�B	��B	��B	��B	��B	�fB	�/B	��B	��B	��B	��B	y+B	o�B	m�B	g�B	a�B	V^B	IB	4�B	)TB	")B	�B	�B	�B	zB	 bB	hB�\B�DB�>B�,B�B�B�B��B��B��B�B�B�BޜBݕBۉB�}B�kB�YB�GB�/B�B�B��B��B��B��B��B��B�aB�zB�sB�nB�hB�PB�>B�8B�&B�B��B��B��B��B��B��B�}B|YB{SByFBw;Bu/Bs"Bi�Be�Bb�B]�B[�BUtBPWBNJBMDBK8BJ2BH&BEB@�B=�B?�B:�B:�B9�B5�B3�B2�B1�B0�B/�B.�B-�B-�B-�B,�B,�B,�B*xB(mB)sB3�B3�B3�B4�B5�B4�B3�B2�B1�B1�B0�B/�B-�B)tB)tB&cB%\B$VB$VB'iB)uB/�B3�B6�B7�B8�B7�B8�B8�B;�B<�B=�B>�BABCBCBB	BDBDBI4BLFBMKBNQBNQBNQBOWBQdBU|BW�BW�B[�B`�Ba�Bb�Bc�Bb�Bd�Bf�Bf�Bf�Bf�Bg�Bh�Bi�Bj�Bj�BlBlBm
Bq"Bq"Bs.Bs.Bs.Bs.BvABzYB}lB}lB�~B��B��B��B��B��B��B��B��B��B�B�B�B�B�5B�5B�GB�MB�SB�YB�lB�xB��B��B��B��B��B��B�B�B�(B�AB�GB�RBۜBߵB��B��B��B��B�
B�B�B�B�/B�:B�@B�FB�YB	�B	�B	B	!B	.B	 4B	%QB	&XB	&XB	'^B	(dB	)jB	)jB	*pB	+vB	/�B	2�B	5�B	6�B	7�B	7�B	:�B	=�B	?�B	CB	D	B	J-B	K3B	PRB	OKB	PRB	SdB	VvB	Z�B	\�B	b�B	e�B	g�B	j�B	l�B	nB	nB	oB	pB	rB	s B	u-B	x>B	yEB	|WB	zKB	w9B	w9B	w9B	{RB	iB	iB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�2B	�>B	�DB	�JB	�WB	�cB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	� B	�,B	�=B	�CB	�JB	�PB	�\B	�bB	�hB	�nB	�uB	�{B	فB	ڇB	ۍB	ܓB	ܓB	ݙB	�B	�B	�B	�B	�B	�G�O�B	��B	��B	��B
�B
�B
�B
%�B
.,B
2B
9�B
AB
FpB
K�B
RB
TwB
W�B
]yB
_7B
g�B
l8B
o�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.35 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.009(+/-0.004) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144102022020411441020220204114410  AO  ARCAADJP                                                                    20200618141336    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200618141336  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200618141336  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114410  IP                  G�O�G�O�G�O�                