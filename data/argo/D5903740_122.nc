CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-07-28T09:15:55Z AOML 3.0 creation; 2016-06-01T00:08:25Z UW 3.1 conversion     
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
resolution        :�o     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150728091555  20160531170826  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               zA   AO  4055_7112_122                   2C  D   APEX                            5374                            041511                          846 @�cJ�؏�1   @�cK���@:fffff�dU\(�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    zA   A   A   @�  @�  A   A   A@  A`  A���A�  A�  A�  A�  A���A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dtl�Dyl�D�3D�\�D���D���D��D�FfD�|�D���D��D�6fD�� D���D�  D�@ Dڃ3D��fD� D�6fD�l�D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�33@�33A��A%��AE��Ae��A���A���A���A���A���Aә�A���A���BffB	ffBffBffB!ffB)ffB1ffB9ffBAffBI��BQffBYffBaffBiffBqffByffB��3B��3B��3B��3B��fB�� B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3Bĳ3Bȳ3B̳3Bг3BԳ3Bس3Bܳ3B�3B�3B�3B�3B�3B��3B��3B��3C Y�CY�CY�CY�CY�C
Y�CY�CY�CY�CY�CY�CY�CY�CY�CY�CY�C Y�C"Y�C$Y�C&Y�C(Y�C*Y�C,Y�C.Y�C0Y�C2Y�C4Y�C6Y�C8Y�C:Y�C<Y�C>Y�C@Y�CBY�CDY�CFY�CHY�CJY�CLY�CNY�CPY�CRY�CTY�CVY�CXY�CZY�C\Y�C^Y�C`Y�CbY�CdY�CfY�ChY�CjY�ClY�CnY�CpY�CrY�CtY�CvY�CxY�CzY�C|Y�C~Y�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�D fD �fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD	fD	�fD
fD
�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD��DfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD fD �fD!fD!�fD"fD"�fD#fD#�fD$fD$�fD%fD%�fD&fD&�fD'fD'�fD(fD(�fD)fD)�fD*fD*�fD+fD+�fD,fD,�fD-fD-�fD.fD.�fD/fD/�fD0fD0�fD1fD1�fD2fD2�fD3fD3�fD4fD4�fD5fD5�fD6fD6�fD7fD7�fD8fD8�fD9fD9�fD:fD:�fD;fD;�fD<fD<�fD=fD=�fD>fD>�fD?fD?�fD@fD@�fDAfDA�fDBfDB�fDCfDC�fDDfDD�fDEfDE�fDFfDF�fDGfDG�fDHfDH�fDIfDI�fDJfDJ�fDKfDK�fDLfDL�fDMfDM�fDNfDN�fDOfDO�fDPfDP�fDQfDQ�fDRfDR�fDSfDS�fDTfDT�fDUfDU�fDVfDV�fDWfDW�fDXfDX�fDYfDY�fDZfDZ�fD[fD[�fD\fD\�fD]fD]�fD^fD^�fD_fD_�fD`fD`�fDafDa�fDbfDb�fDcfDc�fDdfDd�fDefDe�fDffDf�fDgfDg�fDhfDh�fDifDi�fDjfDj�fDkfDk�fDlfDl�fDmfDm�fDnfDn�fDofDo�fDpfDp�fDqfDq�fDrfDr�fDsfDs�fDtfDt�3Dy�3D�fD�h D���D�� D�( D�Q�D�� D�� D� D�A�D��3D�� D�3D�K3DڎfD�љD�3D�A�D�x D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aי�AדuAו�A�t�A�G�A�bAցA�O�A�l�A�jA�?}A�I�A��#A�~�A���A��A�(�A��yA�dZA�I�A�VA�M�A�33A�A�A��A��^A��
A�A�%A�JA��A��/A��A���A�7LA��TA�A���A�9XA�5?A�n�A�7LA�A�ȴA��A�?}A�ȴA�A���A���A�ZA��mA��jA�z�A�O�A��`A�9XA�S�A���A���A���A�A���A�bNA�ƨA�$�A���A��7A�dZA��\A�ffA�p�A��A�1'A���A�M�A���A�r�A�hsA���A�A�A��;A�G�A��/A��A�Q�A�=qA�1'A��HA�C�A��A��FA��+A�E�A��
A�n�A��
A�|�A�7LA��;A�XA�/A�VA�
A��A|�A~-A}7LA}VA|n�A{��A{"�Az�Az��AzI�Az1'AyƨAyx�Ax�uAw�mAw��Aw��Aw�PAwl�Aw\)Aw?}Av��Av��AvM�Aux�At~�At �As�
Ar�Aq�
Aq;dAp��Ao�AnĜAn=qAm��Am�PAmO�Al{Ak&�AiVAg�AeS�AdM�A`�A]�A\�A[�wAX �AU��AT��AT$�AR^5AN��AM��AL=qAK`BAJ^5AH�AG��AF��AD(�AA��A@M�A>��A=�TA=A=�A<^5A;`BA:-A9��A9��A9dZA97LA8�!A7��A7C�A6ȴA6=qA5�A3�A2�+A1��A0�A/��A/�A.jA.n�A-�7A,�A+x�A*�\A(��A'��A'K�A&��A&Q�A%��A$1'A#\)A"��A!�A!p�A!oA �/A �9A -A�A�;AhsA�!A�;A`BA��Ax�Ax�An�A��A�A`BA��AA�Al�AffA��A�hAl�AS�A�uA�wA�DAl�A
�yA
��A
=qA	+A��An�A^5AA�AƨA �A9XA��AA^5A��A �D@���@�V@�/@�I�@���@��\@��
@�@��@�G�@�A�@�S�@�E�@ꗍ@�V@�@�!@�E�@�@�7@��`@�D@�1@�S�@�J@�Z@އ+@�X@�V@ܣ�@�|�@ڇ+@ٺ^@��m@�J@��@ӶF@җ�@�?}@�9X@��@́@���@�j@�(�@���@Ɂ@��@���@�O�@�1'@��H@��#@��^@���@��@�Ĝ@�I�@�K�@��@���@��H@���@�&�@�j@�1'@�|�@�=q@���@�/@�bN@�b@�  @�ƨ@�C�@��+@���@�9X@�;d@�$�@��h@�G�@�%@��@���@���@�Ĝ@��
@�"�@�E�@��h@�X@�%@�z�@��@�|�@�@��R@���@�$�@�x�@�V@�%@�%@���@�r�@�C�@�@��!@�n�@�@�X@�7L@�%@���@� �@���@��@�x�@��`@��@�
=@�ȴ@�=q@��@�X@��j@�I�@�t�@�"�@���@��\@�$�@��@�G�@�V@�Z@��;@�ƨ@��F@���@��@�t�@�K�@��@�^5@��^@���@���@��7@�V@��9@�b@��P@�C�@��@�E�@�@��@��@��@��@�I�@�1'@�A�@�z�@���@���@�r�@�I�@�  @�C�@��y@�^5@���@�`B@��@��/@��/@���@�bN@�  @��@�dZ@�+@���@�^5@��#@���@�?}@�Q�@�(�@�1'@�(�@�;@~�R@}�@}p�@}O�@}/@|�@|(�@|(�@|j@{��@z�@z-@yG�@xr�@x  @w�@xb@x1'@x�@x��@xb@w�w@w
=@vff@u?}@u�@u�@u�@u�@t��@tj@t1@s��@s33@r�!@q�#@pĜ@pbN@o�;@j�\@cdZ@_�@[@P  @H �@B-@>�+@97L@3dZ@.�+@)G�@#t�@E�@�^@�R@t�@V@
-@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aי�AדuAו�A�t�A�G�A�bAցA�O�A�l�A�jA�?}A�I�A��#A�~�A���A��A�(�A��yA�dZA�I�A�VA�M�A�33A�A�A��A��^A��
A�A�%A�JA��A��/A��A���A�7LA��TA�A���A�9XA�5?A�n�A�7LA�A�ȴA��A�?}A�ȴA�A���A���A�ZA��mA��jA�z�A�O�A��`A�9XA�S�A���A���A���A�A���A�bNA�ƨA�$�A���A��7A�dZA��\A�ffA�p�A��A�1'A���A�M�A���A�r�A�hsA���A�A�A��;A�G�A��/A��A�Q�A�=qA�1'A��HA�C�A��A��FA��+A�E�A��
A�n�A��
A�|�A�7LA��;A�XA�/A�VA�
A��A|�A~-A}7LA}VA|n�A{��A{"�Az�Az��AzI�Az1'AyƨAyx�Ax�uAw�mAw��Aw��Aw�PAwl�Aw\)Aw?}Av��Av��AvM�Aux�At~�At �As�
Ar�Aq�
Aq;dAp��Ao�AnĜAn=qAm��Am�PAmO�Al{Ak&�AiVAg�AeS�AdM�A`�A]�A\�A[�wAX �AU��AT��AT$�AR^5AN��AM��AL=qAK`BAJ^5AH�AG��AF��AD(�AA��A@M�A>��A=�TA=A=�A<^5A;`BA:-A9��A9��A9dZA97LA8�!A7��A7C�A6ȴA6=qA5�A3�A2�+A1��A0�A/��A/�A.jA.n�A-�7A,�A+x�A*�\A(��A'��A'K�A&��A&Q�A%��A$1'A#\)A"��A!�A!p�A!oA �/A �9A -A�A�;AhsA�!A�;A`BA��Ax�Ax�An�A��A�A`BA��AA�Al�AffA��A�hAl�AS�A�uA�wA�DAl�A
�yA
��A
=qA	+A��An�A^5AA�AƨA �A9XA��AA^5A��A �D@���@�V@�/@�I�@���@��\@��
@�@��@�G�@�A�@�S�@�E�@ꗍ@�V@�@�!@�E�@�@�7@��`@�D@�1@�S�@�J@�Z@އ+@�X@�V@ܣ�@�|�@ڇ+@ٺ^@��m@�J@��@ӶF@җ�@�?}@�9X@��@́@���@�j@�(�@���@Ɂ@��@���@�O�@�1'@��H@��#@��^@���@��@�Ĝ@�I�@�K�@��@���@��H@���@�&�@�j@�1'@�|�@�=q@���@�/@�bN@�b@�  @�ƨ@�C�@��+@���@�9X@�;d@�$�@��h@�G�@�%@��@���@���@�Ĝ@��
@�"�@�E�@��h@�X@�%@�z�@��@�|�@�@��R@���@�$�@�x�@�V@�%@�%@���@�r�@�C�@�@��!@�n�@�@�X@�7L@�%@���@� �@���@��@�x�@��`@��@�
=@�ȴ@�=q@��@�X@��j@�I�@�t�@�"�@���@��\@�$�@��@�G�@�V@�Z@��;@�ƨ@��F@���@��@�t�@�K�@��@�^5@��^@���@���@��7@�V@��9@�b@��P@�C�@��@�E�@�@��@��@��@��@�I�@�1'@�A�@�z�@���@���@�r�@�I�@�  @�C�@��y@�^5@���@�`B@��@��/@��/@���@�bN@�  @��@�dZ@�+@���@�^5@��#@���@�?}@�Q�@�(�@�1'@�(�@�;@~�R@}�@}p�@}O�@}/@|�@|(�@|(�@|j@{��@z�@z-@yG�@xr�@x  @w�@xb@x1'@x�@x��@xb@w�w@w
=@vff@u?}@u�@u�@u�@u�@t��@tj@t1@s��@s33@r�!@q�#@pĜ@pbN@o�;@j�\@cdZ@_�@[@P  @H �@B-@>�+@97L@3dZ@.�+@)G�@#t�@E�@�^@�R@t�@V@
-@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBXBXBXBXBYBZBcTBgmBp�Br�Bx�B�{B��BXB�{B�9B�LB�}B��B�3B��B��B��B��B��B��B�JB�By�Bv�Bq�BiyBJ�B%�B0!B#�B�B�BJB1B��B��B��B��B��BDB��B  B��B��B\B
=B+BB��B�B�NBǮB��Bz�B_;BP�BD�B;dB.B�B%B�B�;B��BŢB�jB�?B�B��B��B�uB�Bo�BbNB]/BVBM�BF�BB�B?}B=qB<jB6FB-B'�B#�B �B�BuBDBB
��B
��B
�B
�B
�sB
�`B
�NB
�HB
�;B
�B
�B
�)B
�B
��B
��B
��B
��B
��B
��B
ǮB
ĜB
�jB
�FB
�3B
�-B
�-B
�!B
�B
�B
�B
��B
��B
��B
�bB
�JB
�1B
~�B
t�B
n�B
jB
_;B
VB
O�B
I�B
G�B
D�B
8RB
.B
�B
%B	��B	�yB	��B	�'B	��B	��B	�7B	{�B	w�B	p�B	bNB	L�B	H�B	D�B	E�B	C�B	@�B	9XB	2-B	 �B	bB	1B	B��B��B��B��B��B��B��B��B	  B	B	B	B	B	B��B��B�B�B�fB�HB�B��B��B��B�
B��B��BȴB�}B�XB�FB�3B�!B�B�B�B��B��B��B��B��B��B��B��B��B��B��B�uB�bB�JB�%B|�Bx�Bs�Bo�Bm�BjBgmBcTB]/BYBVBVBT�BR�BN�BL�BI�BH�BG�BF�BD�BE�BF�BF�BF�BC�B:^B2-B0!B6FB7LB5?B2-B1'B/B-B+B'�B#�B#�B#�B"�B"�B!�B!�B�B�B�B�B�B�B�B�B�B�B�B�B�B{BuBuBoBhBbBbBVBJBJBDB
=B	7B1B1B1B
=BPBbBhBuB{BoB{B�B�B�B�B�B�B�B�B�B�B�B#�B#�B&�B'�B(�B(�B(�B,B-B-B/B0!B0!B0!B0!B2-B33B8RB;dB>wB@�BA�BA�BB�BE�BG�BO�BR�BS�BVBXBXBYBYBZB\)B_;B`BB`BBaHBbNBdZBdZBe`BdZBe`BiyBl�Bn�Bo�Bq�Bv�B{�B|�B}�B|�B{�B� B�B�B�JB�\B�bB�{B��B��B��B��B��B��B��B��B�B�B�3B�FB�jBĜBƨBǮB��B��B��B��B��B��B�#B�)B�)B�)B�;B�HB�ZB�fB�mB�B�B�B�B�B�B��B��B��B��B	B	B	%B	1B	1B	1B	
=B	JB	hB	uB	{B	�B	�B	�B	�B	�B	 �B	#�B	&�B	)�B	,B	.B	0!B	1'B	2-B	49B	6FB	6FB	6FB	8RB	;dB	@�B	B�B	C�B	C�B	D�B	G�B	I�B	J�B	K�B	L�B	K�B	M�B	Q�B	S�B	VB	W
B	YB	[#B	\)B	_;B	`BB	`BB	bNB	cTB	cTB	cTB	cTB	cTB	dZB	ffB	gmB	iyB	jB	l�B	n�B	q�B	q�B	t�B	�JB	�B	�#B	�B
B
�B
!�B
'�B
0!B
8RB
>wB
E�B
L�B
S�B
ZB
^5B
bNB
hsB
n�B
r�B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BW�BW�BW�BW�BX�BZ Bc6BgQBp�Br�Bx�B�^B˪BW�B�aB�$B�6B�fB�kB�B��B�{B��B��B��B��B�1B�By�Bv�Bq�Bi^BJ�B%�B0B#�B�B�B/BB��B��B��B��B��B%B��B��B��B��B@B
BB �B��B�B�0BǏB�}Bz�B_BP�BD|B;AB-�B�BB�B�B��B�B�GB�B��B��B��B�UB��BoyBb-B]BU�BM�BF�BBpB?\B=PB<JB6$B,�B'�B#�B �B�BUB"B�B
��B
��B
�B
�fB
�QB
�<B
�/B
�(B
�B
��B
��B
�	B
��B
��B
��B
��B
ϼB
̬B
˧B
ǋB
�|B
�JB
�$B
�B
�B
�B
�B
��B
��B
��B
��B
��B
�B
�CB
�)B
�B
~�B
t�B
nyB
j]B
_B
U�B
O�B
I�B
G�B
D|B
83B
-�B
|B
	B	��B	�]B	�mB	�B	��B	��B	�B	{�B	w�B	p�B	b7B	L�B	H�B	D�B	E�B	C�B	@lB	9>B	2B	 �B	NB	B	 �B��B��B��B��B��B��B��B��B��B	 �B	�B	B	�B	 �B��B��B�B�pB�QB�4B�B��B��B��B��B��B;BȠB�hB�DB�0B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�uB�aB�NB�5B�B|�Bx�Bs�Bo�Bm~BjmBg[Bc@B]BYBU�BU�BT�BR�BN�BL�BI�BH�BG�BF�BD�BE�BF�BF�BF�BC�B:NB2B0B63B7<B50B2B1B/B,�B*�B'�B#�B#�B#�B"�B"�B!�B!�B�B�B�B�B�B{B�B{B�BuBnBaBZBiBcBbB]B9B5B6B,B B8B1B
B	%B!BBB
B#B5B;BdBgB[BiBnBtBByBwB�BxB�B�B�B�B#�B#�B&�B'�B(�B(�B(�B+�B,�B,�B/B0B0B0
B0B2B3B8=B;PB>aB@nBArBAtBBzBE�BG�BO�BR�BS�BU�BW�BW�BY BY BZB\B_%B`+B`(Ba/Bb7BdCBdDBeIBdBBeHBi_BlqBn~Bo�Bq�Bv�B{�B|�B}�B|�B{�B�B��B� B�1B�DB�GB�`B�dB�xB��B��B��B��B��B��B��B��B�B�*B�KBāBƊBǏBʤB˨B̱BͶB��B��B�B�	B�B�B�B�(B�<B�HB�OB�cB�rB�pB�yB�B�B��B��B��B��B	�B	�B	B	B	B	B	
B	)B	IB	SB	\B	dB	|B	{B	�B	�B	 �B	#�B	&�B	)�B	+�B	-�B	0B	1B	2B	4B	6#B	6#B	6#B	8/B	;BB	@bB	BlB	CpB	CrB	D|B	G�B	I�B	J�B	K�B	L�B	K�B	M�B	Q�B	S�B	U�B	V�B	X�B	Z�B	\B	_B	`B	`B	b)B	c0B	c0B	c.B	c.B	c0B	d7B	fBB	gHB	iUB	j[B	liB	ntB	q�B	q�B	t�B	�%B	��B	��B	�B
�B
YB
!�B
'�B
/�B
8(B
>MB
EzB
L�B
S�B
Y�B
^B
b#B
hHB
nnB
r�B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.35 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708262016053117082620160531170826  AO  ARCAADJP                                                                    20150728091555    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150728091555  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150728091555  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170826  IP                  G�O�G�O�G�O�                