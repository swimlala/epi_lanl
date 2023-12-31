CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-08-05T00:02:58Z AOML 3.0 creation; 2016-06-01T00:08:19Z UW 3.1 conversion     
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
_FillValue                  0  �|   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �(   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �8   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �<   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �L   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �P   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �T   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �XArgo profile    3.1 1.2 19500101000000  20140805000258  20160531170820  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               WA   AO  4055_7112_087                   2C  D   APEX                            5374                            041511                          846 @�	����1   @�	�r�@
@:�I�^5�d�/��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    WA   A   A   @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���B ffB  B  B  B   B(  B0ffB6ffB?��BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Ci�fCk�fCn  Cp  Cr  Ct  Cv  Cx  Cy�fC|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=fD=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDyl�D��3D�S3D���D��3D�  D�P D�� D��3D�3D�9�D���DǶfD��fD�@ D�y�D�ɚD� D�C3D��D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @0  @�33@�33A��A%��AE��Ae��A���A���A���A���A���A���A���A�B��B	ffBffBffB!ffB)ffB1��B7��BA  BIffBQffBYffBaffBiffBqffByffB��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��fB��3B��3B��3B��3Bĳ3Bȳ3B̀ Bг3BԳ3Bس3Bܳ3B�3B�3B�3B�3B�3B��3B��3B��3C Y�CY�CY�CY�CY�C
Y�CY�CY�CY�CY�CY�CY�CY�CY�CY�CY�C Y�C"Y�C$Y�C&Y�C(Y�C*Y�C,Y�C.Y�C0Y�C2Y�C4Y�C6Y�C8Y�C:Y�C<Y�C>Y�C@Y�CBY�CDY�CFY�CHY�CJY�CLY�CNY�CPY�CRY�CTY�CVY�CXY�CZY�C\Y�C^Y�C`Y�CbY�CdY�CfY�ChY�Cj@ Cl@ CnY�CpY�CrY�CtY�CvY�CxY�Cz@ C|Y�C~Y�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�  C�,�C�,�C�,�D fD �fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD	fD	�fD
fD
�fDfD��DfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD fD �fD!fD!�fD"fD"�fD#fD#�fD$fD$�fD%fD%�fD&fD&�fD'fD'�fD(fD(�fD)fD)�fD*fD*�fD+fD+�fD,fD,�fD-fD-�fD.fD.�fD/fD/�fD0fD0�fD1fD1�fD2fD2�fD3fD3�fD4fD4�fD5fD5�fD6fD6�fD7fD7�fD8fD8�fD9fD9�fD:fD:�fD;fD;�fD<fD<�fD=�D=�fD>fD>�fD?fD?�fD@fD@�fDAfDA�fDBfDB�fDCfDC�fDDfDD�fDEfDE�fDFfDF�fDGfDG�fDHfDH�fDIfDI�fDJfDJ�fDKfDK�fDLfDL�fDMfDM�fDNfDN�fDOfDO�fDPfDP�fDQfDQ�fDRfDR�fDSfDS�fDTfDT�fDUfDU�fDVfDV�fDWfDW�fDXfDX�fDYfDY�fDZfDZ�fD[fD[�fD\fD\�fD]fD]�fD^fD^�fD_fD_�fD`fD`�fDafDa�fDbfDb�fDcfDc�fDdfDd�fDefDe�fDffDf�fDgfDg�fDhfDh�fDifDi�fDjfDj�fDkfDk�fDlfDl�fDmfDm�fDnfDn�fDofDo�fDpfDp�fDqfDq�fDrfDr�fDsfDs�fDtfDt�fDt��Dy�3D��fD�^fD�� D��fD�+3D�[3D��3D��fD�fD�D�D�� D���D��D�K3Dڄ�D���D�3D�NfD� D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A��A��A��A��A��A��A��A��A��TA��#A��/A��HA�ƨA�VAҋDA��A�Q�AΕ�A���A�I�A���A�^5A���A�ȴA�K�A�E�A��A�XA��!A���A�JA�
=A�Q�A��A�`BA�%A�`BA�33A���A���A�33A��RA���A���A��-A�VA�C�A���A�S�A��A�;dA��A��jA�ffA��A��A�VA���A�A�~�A�bA��RA�ffA��A��A���A��TA���A�bNA�I�A�"�A���A�K�A��RA�
=A�A��PA�1'A�(�A�$�A��A�XA��RA�dZA�1A�O�A��hA�{A�ƨA�K�A�&�A���A���A��A�+A�  A���A��!A���A�33A���A�ȴA��A�E�A�1'A�=qA��A���A��\A�C�A�bNA�XA��A�|�A�G�A�A��DA��A�S�A��7A���A~n�A}33A{/Axn�Au��At�AsAqdZAn1'AlM�Ak33Ajv�Ai�-Ah�+Af�9Ae�7AeAdbNAaK�A^jA\JAZ�AYt�AX(�AWS�AV �AUG�AT^5AS/ARn�AQ�AQ/APffAOO�AM33AL��AK�TAJ��AJM�AI\)AH��AG�AF��AF �AE��AE�-AD�AC��AB��A@��A>��A=��A=�#A=�
A=��A<��A:��A9��A8ȴA7��A6��A6�A6�A5��A4�HA3�wA2��A1��A0ĜA0�A/�7A.��A-��A-A,  A+�A*r�A(ZA&�HA&r�A&=qA&1A%A%VA#�FA"~�A!��A!O�A z�A��A�A�A��A{A
=A��A9XA��A�9A�AAXA�yA�FA��A�A1Ax�Al�A`BAS�AK�AC�A�A�\A
�AJA��A�wA�A��A~�A�hA�7A��A$�AbAJA�A�@��y@�?}@�9X@��@��
@��@�ff@��@��h@��@�~�@�Z@�n�@��^@�O�@�@�J@�?}@��@��@��
@�dZ@�R@�O�@߶F@ܛ�@ف@�\)@�-@Չ7@� �@�E�@���@϶F@��@·+@���@�5?@ȃ@��H@�5?@���@��
@�C�@§�@��@���@�33@�
=@��y@���@�@��@��@��@�\)@��!@�M�@�=q@��#@���@��@�bN@��m@��;@���@�t�@�^5@��#@�G�@���@�A�@�l�@�+@���@���@�E�@�`B@��@�bN@�C�@��R@�v�@�-@��@���@���@�j@�A�@���@�dZ@��R@���@���@��`@��@�z�@�1'@�l�@�$�@�5?@�5?@��#@��h@��@�x�@�hs@�O�@�&�@��u@�Z@�9X@�1'@�  @�t�@���@��7@��D@��@�o@��@���@�C�@�~�@��@�J@��@��h@��@�z�@�  @��R@�5?@���@�x�@�O�@�&�@���@��u@� �@��m@��;@�ƨ@�+@��@��\@�V@�$�@��-@��h@�/@���@�Ĝ@�r�@�9X@�  @��;@��
@���@�ƨ@�ƨ@��F@�dZ@���@��@���@�X@��@��9@�(�@��;@���@��@�t�@���@�J@���@��7@�p�@�X@�O�@�O�@�O�@��@��u@�Z@�I�@�(�@� �@��@��@�b@��@�|�@�33@��H@�ȴ@��@��@���@�?}@��9@���@�j@�r�@�I�@�ƨ@��@��@�l�@���@�v�@�M�@�{@��@��T@���@���@���@���@�@��7@�7L@��/@�A�@;d@~ȴ@~��@~{@}@}?}@}V@|��@|�@|z�@{�m@{��@vȴ@n�@dZ@^5?@S�m@L�j@F$�@A��@:-@1��@,�@)�#@'l�@#dZ@�@dZ@��@33@ȴ@\)@dZ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A��A��A��A��A��A��A��A��A��A��TA��#A��/A��HA�ƨA�VAҋDA��A�Q�AΕ�A���A�I�A���A�^5A���A�ȴA�K�A�E�A��A�XA��!A���A�JA�
=A�Q�A��A�`BA�%A�`BA�33A���A���A�33A��RA���A���A��-A�VA�C�A���A�S�A��A�;dA��A��jA�ffA��A��A�VA���A�A�~�A�bA��RA�ffA��A��A���A��TA���A�bNA�I�A�"�A���A�K�A��RA�
=A�A��PA�1'A�(�A�$�A��A�XA��RA�dZA�1A�O�A��hA�{A�ƨA�K�A�&�A���A���A��A�+A�  A���A��!A���A�33A���A�ȴA��A�E�A�1'A�=qA��A���A��\A�C�A�bNA�XA��A�|�A�G�A�A��DA��A�S�A��7A���A~n�A}33A{/Axn�Au��At�AsAqdZAn1'AlM�Ak33Ajv�Ai�-Ah�+Af�9Ae�7AeAdbNAaK�A^jA\JAZ�AYt�AX(�AWS�AV �AUG�AT^5AS/ARn�AQ�AQ/APffAOO�AM33AL��AK�TAJ��AJM�AI\)AH��AG�AF��AF �AE��AE�-AD�AC��AB��A@��A>��A=��A=�#A=�
A=��A<��A:��A9��A8ȴA7��A6��A6�A6�A5��A4�HA3�wA2��A1��A0ĜA0�A/�7A.��A-��A-A,  A+�A*r�A(ZA&�HA&r�A&=qA&1A%A%VA#�FA"~�A!��A!O�A z�A��A�A�A��A{A
=A��A9XA��A�9A�AAXA�yA�FA��A�A1Ax�Al�A`BAS�AK�AC�A�A�\A
�AJA��A�wA�A��A~�A�hA�7A��A$�AbAJA�A�@��y@�?}@�9X@��@��
@��@�ff@��@��h@��@�~�@�Z@�n�@��^@�O�@�@�J@�?}@��@��@��
@�dZ@�R@�O�@߶F@ܛ�@ف@�\)@�-@Չ7@� �@�E�@���@϶F@��@·+@���@�5?@ȃ@��H@�5?@���@��
@�C�@§�@��@���@�33@�
=@��y@���@�@��@��@��@�\)@��!@�M�@�=q@��#@���@��@�bN@��m@��;@���@�t�@�^5@��#@�G�@���@�A�@�l�@�+@���@���@�E�@�`B@��@�bN@�C�@��R@�v�@�-@��@���@���@�j@�A�@���@�dZ@��R@���@���@��`@��@�z�@�1'@�l�@�$�@�5?@�5?@��#@��h@��@�x�@�hs@�O�@�&�@��u@�Z@�9X@�1'@�  @�t�@���@��7@��D@��@�o@��@���@�C�@�~�@��@�J@��@��h@��@�z�@�  @��R@�5?@���@�x�@�O�@�&�@���@��u@� �@��m@��;@�ƨ@�+@��@��\@�V@�$�@��-@��h@�/@���@�Ĝ@�r�@�9X@�  @��;@��
@���@�ƨ@�ƨ@��F@�dZ@���@��@���@�X@��@��9@�(�@��;@���@��@�t�@���@�J@���@��7@�p�@�X@�O�@�O�@�O�@��@��u@�Z@�I�@�(�@� �@��@��@�b@��@�|�@�33@��H@�ȴ@��@��@���@�?}@��9@���@�j@�r�@�I�@�ƨ@��@��@�l�@���@�v�@�M�@�{@��@��T@���@���@���@���@�@��7@�7L@��/@�A�@;d@~ȴ@~��@~{@}@}?}@}V@|��@|�@|z�@{�m@{��@vȴ@n�@dZ@^5?@S�m@L�j@F$�@A��@:-@1��@,�@)�#@'l�@#dZ@�@dZ@��@33@ȴ@\)@dZ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBF�BF�BF�BF�BF�BF�BF�BF�BF�BE�BE�BD�BD�BD�B@�B2-B�BDBB�B�BB��BɺB�'B�LB�^B��B��BĜBǮBŢBǮB�XB��B�oB�1B|�By�B}�B�B�B~�By�Br�BbNB5?B!�BPB+BBB��B��B��B�B�B�B�sB�NB�HB�BB�)B�B��B��B��BɺBĜB�jB�3B�B��B��B��B��B�hB�1B�B�B}�B|�B|�Bx�Bo�BffB`BBYBK�B>wB49B-B#�B�B�B�BbB	7BB  B��B�sB�B��B��B��B�}B��B�uB�JB�+B� Bv�B_;B49BoB
��B
�B
�B
�;B
��B
�}B
��B
z�B
_;B
R�B
?}B
&�B
bB
B	��B	�yB	��B	ĜB	�jB	�?B	�B	��B	��B	�bB	�DB	�%B	u�B	iyB	^5B	YB	Q�B	K�B	F�B	?}B	:^B	49B	/B	)�B	'�B	#�B	�B	�B	uB	bB	JB		7B	1B	B	B��B��B��B��B��B�B�B�fB�HB�BB�BB�;B�5B�)B�B��B��B��B��BȴBƨBĜBB��B�jB�^B�FB�9B�-B�!B�B�B��B��B��B��B��B��B��B��B��B��B�oB�PB�=B�1B�B�Bz�Bv�Br�Bo�Bk�BgmBdZBaHB_;B]/B\)BZBXBVBQ�BM�BJ�BH�BG�BG�BG�BG�BF�BE�BC�B?}B:^B7LB6FB5?B49B2-B0!B-B)�B)�B(�B(�B'�B&�B"�B!�B �B �B�B�B�B�B�B�B�B�B{B{BuBoBbBhBbBVBVB\BVBPBJBDBDBJBPBVBVBVBbBoBuBuBoBoB�B�B�B�B�B�B�B�B�B!�B%�B%�B%�B%�B'�B'�B'�B(�B)�B+B,B,B.B/B2-B5?B7LB7LB7LB6FB8RB:^B<jB=qB=qB@�BA�BA�BB�BD�BG�BH�BI�BL�BN�BO�BP�BR�BVBVBW
BW
BXBYB[#B^5B_;BbNBbNBcTBcTBe`Bk�BjBjBk�Bl�Bm�Bm�Bm�Bm�Bm�Bo�Bp�Bq�Bq�Bq�Bs�Bv�Bz�B� B�B�+B�=B�uB��B��B��B��B��B��B��B�B�!B�XB�qB��BÖBĜBŢBȴB��B��B��B��B��B��B�
B�#B�/B�;B�TB�ZB�sB�B�B�B�B�B�B��B��B��B��B��B��B��B	B	B	B	%B	+B	DB	VB	VB	bB	oB	�B	!�B	$�B	%�B	&�B	'�B	'�B	'�B	&�B	(�B	,B	.B	.B	/B	/B	/B	/B	/B	1'B	2-B	6FB	9XB	;dB	@�B	@�B	@�B	@�B	C�B	C�B	D�B	C�B	D�B	G�B	H�B	I�B	I�B	N�B	Q�B	S�B	VB	XB	XB	YB	YB	YB	YB	YB	ZB	]/B	`BB	e`B	k�B	m�B	m�B	o�B	p�B	r�B	s�B	s�B	s�B	u�B	w�B	{�B	�=B	��B	��B	�5B	��B

=B
�B
 �B
-B
8RB
?}B
D�B
G�B
L�B
R�B
W
B
ZB
bNB
gmB
o�B
u�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BF�BF�BF�BF�BF�BF�BF�BF�BF�BE�BE�BD�BD�BD�B@sB2B�B1B�B�B�,B��BɣB�B�5B�HB�kB�nBĄBǙBŌBǗB�DB��B�UB�B|�By�B}�B��B��B~�By�Br�Bb1B5 B!�B3BB�B �B��B��B��B�B�B�xB�QB�1B�+B� B�B��B��B͵B˩BɝB�~B�KB�B��B��B��B��B��B�CB�B��B��B}�B|�B|�Bx�Bo~BfGB`"BX�BK�B>UB4B,�B#�B�B�BjB@B	B�B��B��B�SB��B��BζBʟB�[B��B�UB�,B�B�Bv�B_B4BPB
��B
��B
�]B
�B
��B
�\B
��B
z�B
_B
R�B
?aB
&�B
DB
�B	��B	�_B	��B	ăB	�QB	�$B	��B	��B	��B	�LB	�+B	�
B	u�B	iaB	^B	Y B	Q�B	K�B	F�B	?hB	:GB	4!B	/B	)�B	'�B	#�B	�B	�B	`B	LB	3B		"B	B	B	�B��B��B��B��B��B�B�pB�RB�4B�-B�,B�%B�#B�B�B��B��BͽB˴BȟBƓBċB�|B�pB�UB�LB�2B�&B�B�B� B��B��B��B��B��B��B��B��B�zB�qB�nB�ZB�<B�,B� B�B��Bz�Bv�Br�Bo�BktBg[BdHBa7B_)B]B\BZBW�BU�BQ�BM�BJ�BH�BG�BG�BG�BG�BF�BE�BC�B?mB:OB7:B64B5-B4'B2B/�B,�B)�B)�B(�B(�B'�B&�B"�B!�B �B �B�B�B�BsBrBeBbBoBOBOBcB^B7B<B8BGB+BHB*B$BB2B3BB@B+B*B,BOBCBcBbB_B]BZB�ByB�B�B�B�B�B�B!�B%�B%�B%�B%�B'�B'�B'�B(�B)�B*�B+�B+�B. B/B2B5*B79B78B7;B6/B8=B:GB<UB=\B=\B@lBAsBAuBB{BD�BG�BH�BI�BL�BN�BO�BP�BR�BU�BU�BV�BV�BW�BY B[B^B_%Bb5Bb7Bc;Bc>BeHBkmBjgBjgBknBlsBmxBmyBmyBmzBmyBo�Bp�Bq�Bq�Bq�Bs�Bv�Bz�B�B�B�B�$B�ZB��B��B��B��B��B��B��B��B�B�;B�TB�fB�xBĀBŃBȘB˩BλB��B��B��B��B��B�B�B�B�7B�<B�TB�gB�lB�xB��B�B�B��B��B��B��B��B��B��B	�B	�B	�B	B	B	"B	5B	8B	?B	NB	B	!�B	$�B	%�B	&�B	'�B	'�B	'�B	&�B	(�B	+�B	-�B	-�B	.�B	.�B	.�B	.�B	.�B	1B	2B	6%B	95B	;AB	@aB	@eB	@aB	@cB	CrB	CsB	DzB	CvB	D{B	G�B	H�B	I�B	I�B	N�B	Q�B	S�B	U�B	W�B	W�B	X�B	X�B	X�B	X�B	X�B	Y�B	]B	`B	e<B	kbB	mlB	mlB	o|B	p�B	r�B	s�B	s�B	s�B	u�B	w�B	{�B	�B	��B	ˡB	�B	��B

B
^B
 �B
,�B
8)B
?TB
DtB
G�B
L�B
R�B
V�B
Y�B
b#B
gCB
ovB
u�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.35 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708202016053117082020160531170820  AO  ARCAADJP                                                                    20140805000258    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140805000258  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140805000258  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170820  IP                  G�O�G�O�G�O�                