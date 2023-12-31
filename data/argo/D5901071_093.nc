CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:17Z UW 3.1 conversion   
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               ]A   AO  20111130141252  20190522121826  1727_5046_093                   2C  D   APEX                            2143                            040306                          846 @ԍ�ݭ`1   @ԍ�s��@7V�u�c���`A�1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�33@�  @���A   A@  A`  A�  A���A�  A�33A�  A�  A�  A�33A�33B  B  B��B��B(  B/��B7��B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�33B�  B�  B�  B���B���B�  B�  B�  B���B���B���C�fC�fC  C  C
  C�fC  C�C  C  C  C  C  C  C  C   C"  C$  C&  C'�fC*  C,  C.  C/�fC1�fC4  C6  C8�C:  C;�fC>  C@  CB  CD�CF  CG�fCJ  CL�CN  CP  CR�CT  CV  CX�CZ  C\  C]�fC_�fCa�fCc�fCf  Ch  Cj  Cl�Cn�Cp  Cq�fCs�fCv  Cx�Cz  C{�fC}�fC�fC��3C��3C�  C��C�  C�  C��3C�  C��C�  C��3C�  C��C�  C�  C�  C��3C��3C�  C��C��C�  C�  C�  C��3C��3C�  C�  C�  C�  C��3C��3C�  C��C�  C�  C�  C��3C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C��C�  C��3C�  C��C�  C�  C��C�  C�  C��C�  C�  C��C�  C�  C��C�  C��3C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C��C�  C��3C�  C��C�  C�  C��C�  C��3C�  D fD � D ��D� D  D� D  D� D  D� D  D� D  D�fDfD� D��Dy�D��D	y�D	��D
� D  D� D��Dy�D  D� D  Dy�D��D� DfD�fDfD� D��Dy�D��D� DfD� D��Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D   D �fD!  D!� D"  D"� D#fD#� D$  D$y�D$��D%� D&  D&�fD'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0�fD1  D1� D2fD2�fD3fD3�fD4fD4� D5  D5� D6  D6y�D6��D7� D8fD8�fD9fD9�fD:  D:� D;  D;�fD<fD<�fD=  D=y�D>  D>� D?  D?� D@  D@� DA  DA� DB  DBy�DC  DC�fDD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DN��DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DV��DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\y�D\��D]� D^  D^� D_  D_� D`  D`� Da  Da� DbfDb�fDc  Dcy�Dd  Dd� De  De� DffDf� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy�fD�6fD�` D��3D��fD�  D�\�D��fD�� D�#3D�\�D���D��fD��D�s3Dک�D��fD�)�D�VfD�D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�33@�  @���A   A@  A`  A�  A���A�  A�33A�  A�  A�  A�33A�33B  B  B��B��B(  B/��B7��B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�33B�  B�  B�  B���B���B�  B�  B�  B���B���B���C�fC�fC  C  C
  C�fC  C�C  C  C  C  C  C  C  C   C"  C$  C&  C'�fC*  C,  C.  C/�fC1�fC4  C6  C8�C:  C;�fC>  C@  CB  CD�CF  CG�fCJ  CL�CN  CP  CR�CT  CV  CX�CZ  C\  C]�fC_�fCa�fCc�fCf  Ch  Cj  Cl�Cn�Cp  Cq�fCs�fCv  Cx�Cz  C{�fC}�fC�fC��3C��3C�  C��C�  C�  C��3C�  C��C�  C��3C�  C��C�  C�  C�  C��3C��3C�  C��C��C�  C�  C�  C��3C��3C�  C�  C�  C�  C��3C��3C�  C��C�  C�  C�  C��3C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C��C�  C��3C�  C��C�  C�  C��C�  C�  C��C�  C�  C��C�  C�  C��C�  C��3C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C��C�  C��3C�  C��C�  C�  C��C�  C��3C�  D fD � D ��D� D  D� D  D� D  D� D  D� D  D�fDfD� D��Dy�D��D	y�D	��D
� D  D� D��Dy�D  D� D  Dy�D��D� DfD�fDfD� D��Dy�D��D� DfD� D��Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D   D �fD!  D!� D"  D"� D#fD#� D$  D$y�D$��D%� D&  D&�fD'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0�fD1  D1� D2fD2�fD3fD3�fD4fD4� D5  D5� D6  D6y�D6��D7� D8fD8�fD9fD9�fD:  D:� D;  D;�fD<fD<�fD=  D=y�D>  D>� D?  D?� D@  D@� DA  DA� DB  DBy�DC  DC�fDD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DN��DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DV��DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\y�D\��D]� D^  D^� D_  D_� D`  D`� Da  Da� DbfDb�fDc  Dcy�Dd  Dd� De  De� DffDf� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy�fD�6fD�` D��3D��fD�  D�\�D��fD�� D�#3D�\�D���D��fD��D�s3Dک�D��fD�)�D�VfD�D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�  A���A�JA��A�bA��A��A��A��A��A� �A�"�A�"�A�"�A�"�A� �A��A��A��A�"�A�$�A�"�A�(�A� �A�bA�VA�A���A��HA԰!A�bNA��AӸRA�Q�A��`A�+A�n�A�dZA���A�;dA�ȴA���A��`Aˡ�A��A��A��A��;A��hA��TA�ZA�S�A�Q�A��A�bNA��PA�oA�v�A���A��uA���A���A�ffA�33A��A��A�
=A�$�A��TA���A�A�p�A��mA��jA�1'A�r�A��!A��A�^5A��DA��TA��PA�v�A�G�A�JA���A�|�A��A�|�A���A�1A�1A��+A�dZA��uA��A�"�A�7LA���A��9A���A��A��A�9XA��A�JA��FA�  A���A�^5A�=qA~VA|E�Az�jAx�Au�Aqx�Ao�Am`BAkAk&�Aj�+AhJAe��Ac�
Aa�;A_VA]�A\JA[7LAZ�!AZ1'AY�mAYK�AX��AX�+AU�AR�AQx�AO�AN�AMhsAK|�AJ�AI�AHJAE�hAD  AB�HAAAA\)A@�!A>��A=\)A;\)A:ZA8�RA77LA5�A5p�A4JA2ffA1t�A0�A/dZA.��A-�;A+�wA+�A*A)�PA(�+A'XA&��A%�A$^5A#;dA!x�A VA��A
=AXA �A�AG�A��AE�A��A33A�!A��A�/AZA�AA�FA�A$�A/A��A�A��A�AȴAdZA��A�hAK�A"�A
�A
��A	�TA	O�Av�A+AA�A�A��A��A�AA9XA�;A&�@���@��@���@�~�@�v�@��@�X@�V@��
@�r�@�^5@���@�@�@�$�@� �@�"�@�R@�=q@��@�D@��@�x�@䛦@�  @��@�K�@◍@�K�@�$�@݁@ܓu@�t�@��`@�J@�E�@�&�@���@���@���@���@Л�@�
=@ͩ�@�X@�7L@�V@��@̬@�b@�ff@��@�+@š�@��@Ĵ9@��@¸R@�?}@�t�@��T@���@��@��@�1'@�|�@��@���@���@�9X@�|�@�+@��@�G�@���@�{@��
@��\@�?}@�V@�ƨ@��#@��m@�5?@��j@�r�@��
@���@�K�@��h@� �@�S�@���@��7@��@�x�@�hs@�X@��@�dZ@�o@�5?@��@��7@�G�@��@��u@�bN@�I�@�1@���@��@�1'@�S�@�
=@��@���@�J@��7@��@��u@�  @��m@�I�@�Q�@��@�  @�S�@�ȴ@���@���@�"�@�S�@�l�@�dZ@�S�@�C�@�
=@���@�$�@���@�X@��@��/@��9@���@�j@�A�@�  @�ƨ@���@�|�@�l�@�l�@�S�@�+@��@��\@���@�p�@��/@��w@�\)@�\)@�+@�~�@���@���@���@�I�@�b@��@��@���@�^5@���@���@��7@�`B@��@���@��@�I�@�I�@�A�@�9X@�1'@�1@��w@���@���@���@��P@���@���@��w@��w@�1@��@��^@�x�@�`B@�G�@��@���@��u@�Q�@�bN@�I�@� �@��@�b@��@��w@��w@��F@�t�@�;d@�+@���@�^5@�E�@���@�p�@���@��#@��T@��#@�x�@��@��9@���@�j@��;@��P@�|�@�
=@�+@�l�@�l�@�C�@��@��\@�~�@�n�@�V@�~�@��\@�V@�v�@�=q@���@��/@��@�Ĝ@��@��@�Z@�w@~��@~V@~E�@�@l�@|��@u/@m��@cC�@\9X@UV@N5?@F�+@>�y@8��@1��@+ƨ@%?}@ ��@`B@�@l�@�@V@
M�@�R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�  A���A�JA��A�bA��A��A��A��A��A� �A�"�A�"�A�"�A�"�A� �A��A��A��A�"�A�$�A�"�A�(�A� �A�bA�VA�A���A��HA԰!A�bNA��AӸRA�Q�A��`A�+A�n�A�dZA���A�;dA�ȴA���A��`Aˡ�A��A��A��A��;A��hA��TA�ZA�S�A�Q�A��A�bNA��PA�oA�v�A���A��uA���A���A�ffA�33A��A��A�
=A�$�A��TA���A�A�p�A��mA��jA�1'A�r�A��!A��A�^5A��DA��TA��PA�v�A�G�A�JA���A�|�A��A�|�A���A�1A�1A��+A�dZA��uA��A�"�A�7LA���A��9A���A��A��A�9XA��A�JA��FA�  A���A�^5A�=qA~VA|E�Az�jAx�Au�Aqx�Ao�Am`BAkAk&�Aj�+AhJAe��Ac�
Aa�;A_VA]�A\JA[7LAZ�!AZ1'AY�mAYK�AX��AX�+AU�AR�AQx�AO�AN�AMhsAK|�AJ�AI�AHJAE�hAD  AB�HAAAA\)A@�!A>��A=\)A;\)A:ZA8�RA77LA5�A5p�A4JA2ffA1t�A0�A/dZA.��A-�;A+�wA+�A*A)�PA(�+A'XA&��A%�A$^5A#;dA!x�A VA��A
=AXA �A�AG�A��AE�A��A33A�!A��A�/AZA�AA�FA�A$�A/A��A�A��A�AȴAdZA��A�hAK�A"�A
�A
��A	�TA	O�Av�A+AA�A�A��A��A�AA9XA�;A&�@���@��@���@�~�@�v�@��@�X@�V@��
@�r�@�^5@���@�@�@�$�@� �@�"�@�R@�=q@��@�D@��@�x�@䛦@�  @��@�K�@◍@�K�@�$�@݁@ܓu@�t�@��`@�J@�E�@�&�@���@���@���@���@Л�@�
=@ͩ�@�X@�7L@�V@��@̬@�b@�ff@��@�+@š�@��@Ĵ9@��@¸R@�?}@�t�@��T@���@��@��@�1'@�|�@��@���@���@�9X@�|�@�+@��@�G�@���@�{@��
@��\@�?}@�V@�ƨ@��#@��m@�5?@��j@�r�@��
@���@�K�@��h@� �@�S�@���@��7@��@�x�@�hs@�X@��@�dZ@�o@�5?@��@��7@�G�@��@��u@�bN@�I�@�1@���@��@�1'@�S�@�
=@��@���@�J@��7@��@��u@�  @��m@�I�@�Q�@��@�  @�S�@�ȴ@���@���@�"�@�S�@�l�@�dZ@�S�@�C�@�
=@���@�$�@���@�X@��@��/@��9@���@�j@�A�@�  @�ƨ@���@�|�@�l�@�l�@�S�@�+@��@��\@���@�p�@��/@��w@�\)@�\)@�+@�~�@���@���@���@�I�@�b@��@��@���@�^5@���@���@��7@�`B@��@���@��@�I�@�I�@�A�@�9X@�1'@�1@��w@���@���@���@��P@���@���@��w@��w@�1@��@��^@�x�@�`B@�G�@��@���@��u@�Q�@�bN@�I�@� �@��@�b@��@��w@��w@��F@�t�@�;d@�+@���@�^5@�E�@���@�p�@���@��#@��T@��#@�x�@��@��9@���@�j@��;@��P@�|�@�
=@�+@�l�@�l�@�C�@��@��\@�~�@�n�@�V@�~�@��\@�V@�v�@�=q@���@��/@��@�Ĝ@��@��@�Z@�w@~��@~V@~E�@�@l�@|��@u/@m��@cC�@\9X@UV@N5?@F�+@>�y@8��@1��@+ƨ@%?}@ ��@`B@�@l�@�@V@
M�@�R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB6FB6FB5?B5?B5?B5?B5?B5?B5?B5?B5?B5?B5?B5?B5?B6FB6FB7LB7LB6FB6FB6FB5?B6FB9XB9XB;dB<jB?}BE�BT�B�=B��B��B�`B�B�B��B��B  B  B��BB  B��B��B�B�)B�9B��B��B��B��B��B�uB�BW
BO�BO�BP�BZB\)BjB�VB�hB�uB�oB�\B�=B�B�B�B�%B�Bu�BffB\)BYBS�BA�B<jB7LB5?B33B/B,B&�B �B�B	7B�B�;B��BÖB��B�PBy�BbNBVBF�B8RB(�B\B
�B
ƨB
�B
�B
iyB
M�B
5?B
�B	��B	�yB	�)B	��B	�'B	��B	�B	t�B	o�B	k�B	dZB	YB	L�B	?}B	2-B	'�B	�B	�B	hB	\B	DB		7B	%B	B��B�B�TB�5B�B��B��BB�wB�^B�9B�-B�B��B��B��B��B��B��B�{B�{B�uB�VB�bB�\B�\B�JB�DB�7B�JB�JB�JB�JB�VB�PB�7B�+B�B�B�B�B~�B}�B}�B|�B|�B|�Bz�Bx�Bv�Bv�Bv�Bt�Bu�Bt�Bt�Bs�Bq�Bp�Bo�Bn�Bn�Bk�BgmBe`BbNBaHB_;B]/B\)BZBZBYBXBW
BT�BR�BP�BN�BL�BK�BI�BH�BF�BD�BC�BA�B?}BA�BE�BF�BG�BH�BH�BH�BH�BF�BE�BE�BE�BC�BC�BF�BE�BF�BE�BD�BD�BC�BB�BC�BD�BC�BD�BC�BA�B?}B?}B>wB<jB:^B7LB5?B49B6FB8RB9XB8RB8RB7LB6FB9XB;dB<jB;dB;dB:^B9XB9XB:^B:^B=qB?}B@�B?}B>wB@�BB�BF�BJ�BJ�BJ�BJ�BO�BR�Be`B}�B�PB�hB��B��B��B��B��B��B��B��B��B��B��B��B�\B�7B�B�B� B}�B~�B�+B�JB�bB��B��B��B��B��B��B��B��B��B��B�B�3B�3B�?B�XB�dB�dB�dB�}B�qB�dB�jB�wB�wB��BȴB��B��B��B�B�;B�fB�sB�yB�B�B�B�B�B��B��B	  B	B	B	B	%B		7B	DB	\B	hB	oB	oB	uB	{B	�B	�B	�B	�B	 �B	!�B	"�B	#�B	$�B	&�B	&�B	)�B	,B	-B	-B	0!B	0!B	0!B	0!B	/B	/B	33B	5?B	7LB	8RB	<jB	B�B	D�B	G�B	J�B	M�B	M�B	O�B	P�B	Q�B	XB	`BB	aHB	aHB	aHB	aHB	cTB	gmB	jB	jB	jB	p�B	r�B	s�B	u�B	w�B	�B	�DB	�PB	�VB	�\B	�bB	�oB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�3B	�9B	�9B	�9B	�9B	�?B	�9B	�9B	�LB	�XB	�^B	�dB	�dB	�jB	�qB	�wB	�wB	��B	��B	��B	B	B	B	��B	B	B	ĜB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	�5B	�`B	��B	��B
uB
�B
%�B
2-B
=qB
D�B
J�B
O�B
R�B
W
B
\)B
_;B
cTB
iyB
n�B
r�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B6FB6FB5?B5?B5?B5?B5?B5?B5?B5?B5?B5?B5?B5?B5?B6FB6FB7LB7LB6FB6FB6FB5?B7LB9XB9XB;dB=qBA�BH�BXB�JB�B��B�B�B��B��BBB
=BVBJBoB�BhB	7B��BB��B��B��B��B��B��B��BffBVBT�B[#BaHBbNBm�B�{B��B��B��B�uB�\B�JB�JB�VB��B�oB� Bq�BhsBm�BffBH�B?}B9XB8RB6FB33B2-B0!B)�B/B�B��B�B�fB�B�qB��B�\Bk�BdZBS�BG�BB�B-B	7B
�ZB
ǮB
��B
~�B
dZB
R�B
49B

=B	��B	�B	�mB	ǮB	��B	�oB	� B	u�B	t�B	v�B	iyB	^5B	O�B	D�B	49B	"�B	�B	�B	uB	VB	VB	DB	1B	VB	B�B�yB�HB�)B�
B��BȴBǮBÖB�qB�?B�'B�B�B�B��B��B��B��B��B��B��B��B��B�uB�oB�hB�uB�uB��B�bB��B�oB�hB�\B�DB�JB�DB�JB�=B�%B�B�B�1B�B~�Bz�By�B{�Bz�By�Bz�Bz�B{�Bv�Bs�Br�Bs�Bt�Bt�Bs�Bq�Bk�BgmBffBffBffBbNB`BB\)B[#BZB[#BZBXBYBXBS�BP�BN�BL�BL�BL�BH�BE�BE�BH�BH�BG�BH�BI�BJ�BK�BK�BM�BQ�BL�BJ�BM�BP�BL�BL�BI�BG�BG�BH�BG�BH�BH�BG�BE�BF�BF�BG�BJ�BC�BA�B<jBA�B7LB@�B@�B:^B9XB9XB8RB9XB9XB;dB=qB;dB<jB<jB;dB:^B9XB9XB:^B@�BB�BA�B@�B?}BC�BF�BH�BF�BJ�BK�BJ�BJ�BO�BK�Be`B}�B�PB�uB��B��B��B��B�B��B��B��B��B��B��B��B�{B�PB�B�B�B�B�B�JB�\B�{B��B��B��B��B��B��B��B��B��B��B�B�3B�3B�FB�^B�dB�wB��BB��B�qB�qB�}B�}BB��B��B��B�B�B�5B�fB�yB�B�B�B�B�B�B��B��B	  B	B	B	B	+B	DB	DB	\B	oB	oB	oB	uB	{B	�B	�B	�B	�B	 �B	!�B	"�B	#�B	%�B	'�B	(�B	,B	-B	/B	0!B	1'B	0!B	0!B	2-B	/B	1'B	49B	6FB	8RB	:^B	>wB	C�B	E�B	H�B	K�B	M�B	N�B	P�B	Q�B	R�B	YB	`BB	aHB	aHB	aHB	bNB	dZB	gmB	jB	jB	jB	p�B	r�B	s�B	u�B	v�B	}�B	�DB	�VB	�VB	�\B	�hB	�oB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�3B	�9B	�?B	�?B	�?B	�?B	�?B	�9B	�LB	�XB	�^B	�jB	�jB	�jB	�qB	�wB	�wB	��B	��B	��B	ÖB	ÖB	ÖB	��B	B	B	ĜB	ǮB	��B	��B	��B	��B	��B	��B	��B	�5B	�`B	��B	��B
uB
�B
%�B
2-B
=qB
D�B
J�B
O�B
R�B
W
B
\)B
_;B
dZB
iyB
n�B
r�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<�t�<���<��
<���=C�<e`B<#�
<#�
<#�
<#�
<#�
<�o<��<u<#�
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
<u<�C�<#�
<49X<D��<��
<�t�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�1<�1<T��<49X<�t�<�9X<��
<T��<�1<#�
<e`B<T��<u<���<�h<�<�h<�`B<�j<�1<�9X<�h<���<u<e`B<�t�<���<�9X<e`B<e`B<49X<#�
<#�
<�t�<�o<�C�<�o<�t�<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<���<���<49X<49X<#�
<#�
<D��<#�
<#�
<D��<u<49X<#�
<#�
<#�
<#�
<T��<T��<D��<#�
<D��<49X<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
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
<49X<#�
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
<49X<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447062012010314470620120103144706  AO  ARGQ                                                                        20111130141252  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130141252  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144706  IP                  G�O�G�O�G�O�                