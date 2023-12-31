CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:45Z UW 3.1 conversion   
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
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130143353  20190522121828  1727_5046_192                   2C  D   APEX                            2143                            040306                          846 @�̦�@1   @��8�@5-V�c�/��w1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @&ff@y��@�  A   A   A@  Aa��A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8ffB@  BH  BP  BX  B_��Bg��Bo��Bw��B�  B�33B�  B���B�  B�  B�  B�  B���B���B�  B�  B�  B���B�  B�  B���B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC  C�C�C�C  C  C  C�fC   C"�C$  C&  C(  C*  C,  C.  C0  C2  C4  C5�fC7�fC:  C<  C>  C@�CB�CD�CF�CH�CJ�CL  CM�fCP  CR�CT�CV  CX  CY�fC\  C^  C`  Cb  Cd  Cf�Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv�Cx�Cz  C|  C~  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C�  C��C��C�  C�  C�  C�  C��C�  C�  C��3C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��C�  C�  C�  C�  C��3C��3C��3C��3C��3C�  C��C�  C�  C�  C�  C�  C��3C�  C��C�  C��3C�  C��C�  C��3C�  C�  C��3C�  C�  C��C��C�  C�  C�  C��C�  C��3C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C��3C�  C�  C��3C��C�  C�  C�  C�  C�  C��C��C�  C�  C��C�  C�  C�  C��3C��3C�  C�  C�  C�  C��C�  C��3C��3C��3C�  C��D   D y�D  D� DfD� D��Dy�D  D� D  D� D  D� D  D� D  Dy�D	  D	� D	��D
� DfD� D  D� D  Dy�D  D�fD  D� D  Dy�D  D�fDfD� D  D� D  Dy�D��D� DfD� D  Dy�D  D�fD  Dy�D  D� D  D�fD��Dy�D  D� D  D�fD  D� D   D � D!  D!�fD"  D"� D#  D#y�D$  D$� D$��D%� D&  D&� D'fD'� D(  D(�fD)  D)� D*  D*� D+  D+� D,  D,�fD-fD-� D.  D.� D/fD/� D0  D0�fD1  D1� D2  D2� D3  D3�fD4  D4� D5  D5�fD6  D6� D7fD7� D8  D8� D9  D9� D:  D:� D;  D;�fD<  D<� D=  D=� D>  D>y�D?  D?�fD@  D@� DA  DA�fDBfDB�fDCfDC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DIfDI� DI��DJy�DJ��DK� DL  DL� DL��DM� DNfDN� DO  DO�fDP  DP� DQ  DQ�fDR  DR� DS  DS� DT  DT�fDU  DUy�DU��DVy�DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]y�D^  D^� D_  D_� D`fD`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Diy�Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dm��Dn� Do  Do� Dp  Dpy�Dq  Dq� Dr  Dr� Ds  Ds� Ds��Dy��D�0 D�s3D��fD��3D�fD�ffD��fD��fD�&fD�p D��3D��3D�  D�l�DڦfD��fD�0 D�p D�3D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @,��@�  @�33A��A!��AA��Ac33A���A���A���A���A���A���A���A���B ffBffBffBffB ffB(ffB0ffB8��B@ffBHffBPffBXffB`  Bh  Bp  Bx  B�33B�ffB�33B�  B�33B�33B�33B�33B�  B�  B�33B�33B�33B�  B�33B�33B�  B�33B�33B�33B�33B�33B�33B�  B�33B�33B�33B�33B�33B�  B�  B�  C   C  C  C  C  C
  C  C  C�C33C33C33C�C�C�C  C �C"33C$�C&�C(�C*�C,�C.�C0�C2�C4�C6  C8  C:�C<�C>�C@33CB33CD33CF33CH33CJ33CL�CN  CP�CR33CT33CV�CX�CZ  C\�C^�C`�Cb�Cd�Cf33Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv33Cx33Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C�  C�  C�  C�  C��C��C��C��C��C��C��C�  C�  C��C��C��C��C��C��C�  C�  C�  C�  C�  C��C��C��C��C��C��C��C�  C��C��C��C�  C��C��C��C�  C��C��C�  C��C��C��C��C��C��C��C��C��C�  C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C�  C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C�  C��C��C��C��C��C��C�  C�  C�  C��C��D fD � DfD�fD�D�fD  D� DfD�fDfD�fDfD�fDfD�fDfD� D	fD	�fD
  D
�fD�D�fDfD�fDfD� DfD��DfD�fDfD� DfD��D�D�fDfD�fDfD� D  D�fD�D�fDfD� DfD��DfD� DfD�fDfD��D  D� DfD�fDfD��DfD�fD fD �fD!fD!��D"fD"�fD#fD#� D$fD$�fD%  D%�fD&fD&�fD'�D'�fD(fD(��D)fD)�fD*fD*�fD+fD+�fD,fD,��D-�D-�fD.fD.�fD/�D/�fD0fD0��D1fD1�fD2fD2�fD3fD3��D4fD4�fD5fD5��D6fD6�fD7�D7�fD8fD8�fD9fD9�fD:fD:�fD;fD;��D<fD<�fD=fD=�fD>fD>� D?fD?��D@fD@�fDAfDA��DB�DB��DC�DC�fDDfDD�fDEfDE�fDFfDF�fDGfDG�fDHfDH�fDI�DI�fDJ  DJ� DK  DK�fDLfDL�fDM  DM�fDN�DN�fDOfDO��DPfDP�fDQfDQ��DRfDR�fDSfDS�fDTfDT��DUfDU� DV  DV� DWfDW�fDXfDX�fDYfDY�fDZfDZ�fD[fD[�fD\fD\�fD]fD]� D^fD^�fD_fD_�fD`�D`�fDafDa�fDbfDb�fDcfDc�fDdfDd�fDefDe�fDffDf�fDgfDg�fDhfDh�fDifDi� DjfDj�fDkfDk�fDlfDl�fDmfDm�fDn  Dn�fDofDo�fDpfDp� DqfDq�fDrfDr�fDsfDs�fDt  Dy�3D�33D�vfD���D��fD��D�i�D���D��D�)�D�s3D��fD��fD�#3D�p Dک�D���D�33D�s3D�fD�ٚ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aŗ�A�{A�~�A�`BA�S�A�G�A�A�A�=qA�;dA�7LA�5?A�1'A�(�A�$�A�ȴA§�A��TA�$�A�A���A���A�t�A�O�A�I�A�A�A�~�A�A�A�^5A� �A���A���A�=qA���A���A�?}A��HA�z�A��A��
A�v�A���A�
=A�G�A�C�A�
=A��#A�$�A�t�A��A�A��yA�&�A�x�A��A�ƨA���A���A�?}A���A��A�5?A�l�A��A�ZA�5?A�{A��yA��A�7LA��yA��DA�;dA�  A���A�/A�dZA���A���A��A��A��A�|�A���A�9XA���A��-A�1'A���A�5?A�|�A��A�C�A���A�C�A�|�A�7LA�{A��yA��-A�ƨA�jA��
A���A���A�bNA|M�A{��Az��Az�Ay�Awp�Au�mAu�AtbAq%Am��Ah�9AdbAcO�Aa�hA^{A[�mA[?}AZr�AXZAU�^AS33AQ�FAPjAN��AL=qAK��AI�FAHA�AE�mAD�/AC?}A?|�A=�A8��A7A7`BA7
=A6ZA5�-A5C�A3�mA0ZA*�HA)�-A)�A(�A(ZA&�A$��A$n�A$  A#+A"Q�A!��A!|�A!\)A!?}A!"�A ��A ��A �A��A|�AhsA33A�Ar�AbA�A��A�PA�/A9XA(�Az�A��A�A�wA&�A��AQ�A�A��A��Ap�AC�AG�A?}A/A+A"�A%A�HAffA�^A�A
��A
n�A�HA�AjA��A�7A\)AS�A?}A9XA�A�7AC�A�uA�AdZA V@�n�@�x�@��@��@�C�@�o@��@�S�@��+@�@��@��@�&�@��;@���@�@�1@��@�"�@�@��#@���@�j@�Z@�bN@�Z@�Q�@�(�@�$�@�@�r�@� �@�33@���@��y@ج@�b@�  @�l�@�@�o@�+@��@�~�@�@ա�@�x�@�O�@�%@ԓu@�1@�"�@�C�@˶F@�$�@ɉ7@�Ĝ@Ƨ�@�r�@�1@�5?@��@���@���@��j@�ƨ@���@�+@���@��^@���@��@���@���@��@�l�@�K�@�C�@�;d@�"�@��@�E�@��T@�X@��@���@�z�@��
@�t�@�
=@��@��R@���@�~�@�^5@�M�@�=q@���@��`@��j@��9@�l�@�E�@�hs@���@��9@�9X@��
@��F@��P@�v�@�-@�$�@���@��-@�/@��@�9X@���@��\@��+@�n�@�=q@�@��7@�`B@�&�@�z�@�1'@�b@���@��w@���@���@��@�l�@�K�@�C�@�C�@��@��H@�~�@�M�@�M�@�E�@�=q@�$�@�@��@�r�@�|�@�S�@�K�@��@��+@��7@��@��/@�Ĝ@��j@��9@��u@�A�@��;@�t�@�C�@�K�@��@�@��H@��R@�V@�J@��@�@���@���@���@��@�X@�X@�O�@�&�@��9@��@�I�@�9X@�1'@�(�@�1@���@�C�@�+@���@��\@�ff@�^5@�$�@�`B@�%@��@���@���@���@�%@���@��D@�b@���@��@��@�|�@�+@��@���@�5?@���@�bN@�(�@�1@��@�b@�@��@��@���@���@�~�@�@���@���@���@���@���@�@���@��h@�x�@�hs@�O�@�?}@��@��@�z�@�j@�Q�@�1'@�1@��m@��F@�;d@�@�ȴ@�~�@��-@�`B@�?}@�&�@��@���@�z�@�Q�@�9X@�1'@�1@���@��@��
@�V@��@x�`@o�@g+@`r�@Z-@S��@Mp�@E�@@1'@8b@1�^@'l�@!X@p�@"�@{@�F@
=@  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aŗ�A�{A�~�A�`BA�S�A�G�A�A�A�=qA�;dA�7LA�5?A�1'A�(�A�$�A�ȴA§�A��TA�$�A�A���A���A�t�A�O�A�I�A�A�A�~�A�A�A�^5A� �A���A���A�=qA���A���A�?}A��HA�z�A��A��
A�v�A���A�
=A�G�A�C�A�
=A��#A�$�A�t�A��A�A��yA�&�A�x�A��A�ƨA���A���A�?}A���A��A�5?A�l�A��A�ZA�5?A�{A��yA��A�7LA��yA��DA�;dA�  A���A�/A�dZA���A���A��A��A��A�|�A���A�9XA���A��-A�1'A���A�5?A�|�A��A�C�A���A�C�A�|�A�7LA�{A��yA��-A�ƨA�jA��
A���A���A�bNA|M�A{��Az��Az�Ay�Awp�Au�mAu�AtbAq%Am��Ah�9AdbAcO�Aa�hA^{A[�mA[?}AZr�AXZAU�^AS33AQ�FAPjAN��AL=qAK��AI�FAHA�AE�mAD�/AC?}A?|�A=�A8��A7A7`BA7
=A6ZA5�-A5C�A3�mA0ZA*�HA)�-A)�A(�A(ZA&�A$��A$n�A$  A#+A"Q�A!��A!|�A!\)A!?}A!"�A ��A ��A �A��A|�AhsA33A�Ar�AbA�A��A�PA�/A9XA(�Az�A��A�A�wA&�A��AQ�A�A��A��Ap�AC�AG�A?}A/A+A"�A%A�HAffA�^A�A
��A
n�A�HA�AjA��A�7A\)AS�A?}A9XA�A�7AC�A�uA�AdZA V@�n�@�x�@��@��@�C�@�o@��@�S�@��+@�@��@��@�&�@��;@���@�@�1@��@�"�@�@��#@���@�j@�Z@�bN@�Z@�Q�@�(�@�$�@�@�r�@� �@�33@���@��y@ج@�b@�  @�l�@�@�o@�+@��@�~�@�@ա�@�x�@�O�@�%@ԓu@�1@�"�@�C�@˶F@�$�@ɉ7@�Ĝ@Ƨ�@�r�@�1@�5?@��@���@���@��j@�ƨ@���@�+@���@��^@���@��@���@���@��@�l�@�K�@�C�@�;d@�"�@��@�E�@��T@�X@��@���@�z�@��
@�t�@�
=@��@��R@���@�~�@�^5@�M�@�=q@���@��`@��j@��9@�l�@�E�@�hs@���@��9@�9X@��
@��F@��P@�v�@�-@�$�@���@��-@�/@��@�9X@���@��\@��+@�n�@�=q@�@��7@�`B@�&�@�z�@�1'@�b@���@��w@���@���@��@�l�@�K�@�C�@�C�@��@��H@�~�@�M�@�M�@�E�@�=q@�$�@�@��@�r�@�|�@�S�@�K�@��@��+@��7@��@��/@�Ĝ@��j@��9@��u@�A�@��;@�t�@�C�@�K�@��@�@��H@��R@�V@�J@��@�@���@���@���@��@�X@�X@�O�@�&�@��9@��@�I�@�9X@�1'@�(�@�1@���@�C�@�+@���@��\@�ff@�^5@�$�@�`B@�%@��@���@���@���@�%@���@��D@�b@���@��@��@�|�@�+@��@���@�5?@���@�bN@�(�@�1@��@�b@�@��@��@���@���@�~�@�@���@���@���@���@���@�@���@��h@�x�@�hs@�O�@�?}@��@��@�z�@�j@�Q�@�1'@�1@��m@��F@�;d@�@�ȴ@�~�@��-@�`B@�?}@�&�@��@���@�z�@�Q�@�9X@�1'@�1@���@��@��
@�V@��@x�`@o�@g+@`r�@Z-@S��@Mp�@E�@@1'@8b@1�^@'l�@!X@p�@"�@{@�F@
=@  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBo�Bn�Bn�Bn�Bn�Bn�Bn�Bn�Bn�Bn�Bn�Bn�Bn�Bm�Bs�B�bB��B�qB�qB�qB�qB�wBĜB��B��B��B��B��B��B�
B�5B�5B�B�B��B��B��B��B��BɺB�RB��B�FBÖBǮBĜB�wB��B�=Bs�Bk�BL�B<jB5?B0!B/B-B(�B%�B�B1BB��B��B�B�B�B�HB�B�B��B��BƨB�jB�3B��B��B�oB�=B}�Bw�Br�Bk�BbNB]/BXBQ�BI�BB�B7LB �BDB
�B
�NB
�
B
��B
��B
ɺB
��B
�'B
�\B
n�B
ZB
J�B
9XB
 �B
�B
�B
uB
1B	��B	�B	�B	�HB	��B	�^B	��B	�1B	�B	r�B	`BB	Q�B	L�B	C�B	9XB	+B	"�B	�B	uB	JB	B	  B��B�B�B�TB�)B��BĜB�}B�XB�FB�9B�'B�B��B��B�{B�bB�JB�DB�1B�B�B�B�B�B�B�B�B�B�B�B� B� B~�B~�B~�B~�B}�B|�Bz�By�Bv�Bu�Bt�Bt�Br�Bo�Bn�Bs�Bp�Bq�Bq�Br�Bs�Br�Bt�Bt�Bt�Bt�Bt�Bt�Bt�Bt�Bs�Bs�Br�Bq�Bp�Bp�Bp�Bo�Bm�BjBgmBiyBjBjBjBiyBhsBiyBhsBgmBe`BdZBbNBaHB_;B_;B^5B^5B]/B\)BZBZBZBXBT�BT�BYBYBZBYB[#B\)B\)B\)B\)B^5BaHBbNBaHBaHBaHB`BB_;Be`Bk�Bl�Bm�Bq�Bu�B{�By�Bx�Bx�B{�B~�B�B�%B�+B�+B�+B�+B�1B�7B�1B�7B�7B�1B��B��B��B��B��B��B�B�XB�jB�wB��B��BĜBǮBǮBǮBƨBŢBĜBÖBBǮB��B��B��B�B�B�B�B�
B�
B�
B�B�B�B�5B�BB�ZB�fB�sB�yB�yB�yB�B�B�B�B��B�B��B��B	B	%B	%B	
=B	JB	JB	JB	{B	�B	�B	�B	�B	�B	!�B	$�B	/B	0!B	/B	/B	0!B	33B	49B	5?B	7LB	;dB	=qB	>wB	?}B	@�B	A�B	B�B	B�B	C�B	D�B	D�B	D�B	E�B	G�B	J�B	K�B	L�B	M�B	M�B	N�B	O�B	XB	]/B	cTB	e`B	ffB	ffB	hsB	m�B	q�B	r�B	s�B	t�B	v�B	y�B	~�B	�B	�%B	�+B	�+B	�7B	�7B	�=B	�DB	�PB	�\B	�bB	�oB	�oB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�-B	�3B	�3B	�?B	�^B	�wB	��B	B	B	B	ĜB	ŢB	ƨB	ǮB	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�#B	�)B	�/B	�5B	�5B	�5B	�HB	�TB	�TB	�ZB	�`B	�fB	�fB	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B
%B
VB
�B
�B
+B
2-B
9XB
@�B
F�B
K�B
N�B
ZB
_;B
dZB
gmB
n�B
n�B
r�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Bu�Br�Bo�Bn�Bn�Bn�Bn�Bn�Bn�Bn�Bn�Bn�Bo�Bq�B}�B��B�B�wB�wB�}B��B�}BŢB��B�)B�/B�#B�#B�`B�B�B�`B�NB�5B�B�B�B��B�B��BƨB��B�LBƨB��B��B��B�B��B�B{�BS�B@�B8RB2-B1'B33B2-B6FB1'BbB+BB��B��B��B��B�sB�5B�B��B��B��BƨB�qB�9B��B��B�oB�B{�By�Br�BffBaHB_;BYBP�BO�BL�B33B�B
��B
�B
�B
��B
��B
��B
��B
��B
�B
~�B
hsB
\)B
N�B
%�B
!�B
�B
�B
oB
+B	��B	��B	��B	�yB	�#B	�dB	�hB	�\B	�1B	l�B	XB	VB	T�B	K�B	:^B	.B	$�B	�B	�B	PB	PB	B	  B��B�B�B�TB�BBƨB�jB�^B�^B�LB�?B�RB�XB�3B��B�bB�\B�\B�hB�bB�%B�%B�+B�%B�B�B�B�B�B�B�B�B�B� B� B� B� B�B� B�B�B}�B{�B{�B�B�By�Bw�Bx�Bu�Bv�Bv�Bu�Bu�Bu�Bu�Bu�Bt�Bt�Bu�Bt�Bt�Bt�Bt�Bu�Bu�Bt�Bt�Bt�Bw�Bs�Bn�Bm�Bl�Bk�Bk�Bl�Bn�Bm�BjBjBjBiyBgmBiyBffBbNBaHB`BB`BB_;BaHB`BB^5B^5BaHBbNB_;B^5B_;B_;B]/B^5B^5B^5B`BBaHBbNBbNBaHBbNBbNBcTBe`BiyBl�Bn�Bq�Bw�B~�B�B{�Bx�Bz�B|�B~�B�B�%B�+B�7B�1B�1B�7B�DB�DB�JB�bB��B��B��B��B��B��B��B�-B�wB�qB�}B��BĜBĜBȴBɺBɺBƨBɺBĜBÖBƨB��B�B��B��B�B�
B�B�B�B�
B�B�B�#B�B�;B�HB�`B�fB�sB�yB�yB�yB�B�B�B��B��B��B��B��B	B	+B	1B	
=B	PB	PB	JB	�B	�B	�B	�B	�B	 �B	!�B	(�B	0!B	0!B	0!B	0!B	1'B	49B	49B	5?B	9XB	<jB	=qB	>wB	@�B	@�B	A�B	B�B	B�B	C�B	D�B	D�B	D�B	F�B	H�B	K�B	K�B	L�B	M�B	M�B	O�B	R�B	ZB	`BB	dZB	e`B	gmB	hsB	hsB	n�B	r�B	r�B	s�B	t�B	v�B	z�B	~�B	�B	�+B	�+B	�+B	�7B	�7B	�=B	�JB	�VB	�\B	�hB	�oB	�oB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�-B	�3B	�3B	�FB	�^B	�wB	��B	B	B	ÖB	ŢB	ƨB	ǮB	ɺB	��B	��B	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�#B	�)B	�/B	�5B	�;B	�;B	�HB	�TB	�TB	�ZB	�`B	�fB	�mB	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B
B
VB
�B
�B
+B
2-B
9XB
@�B
F�B
K�B
N�B
ZB
_;B
dZB
gmB
n�B
n�B
r�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<49X<e`B<���<���<e`B<#�
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
<�t�<D��<�o<e`B<�o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�o<�9X<#�
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
<D��<#�
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
<T��<�1<�t�<���<T��<#�
<#�
<#�
<#�
<#�
<�C�<�h<�`B<�C�<u<�C�<�1<#�
<#�
<#�
<#�
<49X<#�
<#�
<D��<�1<���=o<ě�<#�
<e`B<�1<D��<#�
<#�
<�C�<�t�<u<49X<49X<D��<�o<#�
<T��<T��<u<#�
<�o<�j<�t�<�/<#�
<#�
<#�
<#�
<#�
<#�
<�o<���<�<#�
<#�
<#�
<#�
<D��<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<�t�<��
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.1 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447412012010314474120120103144741  AO  ARGQ                                                                        20111130143353  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143353  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144741  IP                  G�O�G�O�G�O�                