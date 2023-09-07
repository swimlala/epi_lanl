CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:14:26Z AOML 3.0 creation; 2016-08-07T21:51:13Z UW 3.1 conversion     
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
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150226221426  20160807145113  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5287_9017_024                   2C  D   APEX                            6529                            072314                          846 @�+�D�� 1   @�+�����@2+I��dK333331   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�33B�  B�  B�33Bי�B���B���B�  B�  B�  B�  B�33B���B�33B���C  C  C  C  C
  C�fC  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<�C>  C@  CB  CD  CF�CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`�Cb�Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?y�D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy� D�fD�L�D���D���D���D�FfD�vfD���D� D�33D�p D��3D�3D�9�D�p D�ɚD��D�<�D��D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�ff@�33A��A%��AE��Ae��A���A���A���A���A���A���A���A���BffB	ffBffBffB!ffB)ffB1ffB9ffBAffBIffBQffBYffBaffBiffBqffByffB��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��fB��3B��3B��3Bĳ3B��fB̳3Bг3B��fB�L�B܀ B�� B�3B�3B�3B�3B��fB�� B��fC @ CY�CY�CY�CY�C
Y�C@ CY�CY�CY�CY�CY�CY�CY�CY�CY�C Y�C"Y�C$Y�C&Y�C(Y�C*Y�C,Y�C.Y�C0Y�C2Y�C4Y�C6Y�C8Y�C:Y�C<s4C>Y�C@Y�CBY�CDY�CFs4CHY�CJY�CLY�CNY�CPY�CRY�CTY�CVY�CXY�CZY�C\Y�C^Y�C`s4Cbs4CdY�CfY�ChY�CjY�ClY�CnY�CpY�CrY�CtY�CvY�CxY�CzY�C|Y�C~Y�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�9�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�D fD �fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD	fD	�fD
fD
�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD fD �fD!fD!�fD"fD"�fD#fD#�fD$fD$�fD%fD%�fD&fD&�fD'fD'�fD(fD(�fD)fD)�fD*fD*�fD+fD+�fD,fD,�fD-fD-�fD.fD.�fD/fD/�fD0fD0�fD1fD1�fD2fD2�fD3fD3�fD4fD4�fD5fD5�fD6fD6�fD7fD7�fD8fD8�fD9fD9�fD:fD:�fD;fD;�fD<fD<�fD=fD=�fD>fD>�fD?fD?� D@fD@�fDAfDA�fDBfDB�fDCfDC�fDDfDD�fDEfDE�fDFfDF�fDGfDG�fDHfDH�fDIfDI�fDJfDJ�fDKfDK�fDLfDL�fDMfDM�fDNfDN�fDOfDO�fDPfDP�fDQfDQ�fDRfDR�fDSfDS�fDTfDT�fDUfDU�fDVfDV�fDWfDW�fDXfDX�fDYfDY�fDZfDZ�fD[fD[�fD\fD\�fD]fD]�fD^fD^�fD_fD_�fD`fD`�fDafDa�fDbfDb�fDcfDc�fDdfDd�fDefDe�fDffDf�fDgfDg�fDhfDh�fDifDi�fDjfDj�fDkfDk�fDlfDl�fDmfDm�fDnfDn�fDofDo�fDpfDp�fDqfDq�fDrfDr�fDsfDs�fDtfDt�fDu3Dy�fD��D�X D�� D�� D��D�Q�D���D�� D�3D�>fD�{3D��fD�fD�D�D�{3D���D� D�H D� D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�-A�33A�33A�5?A�5?A�7LA�7LA�9XA�7LA�;dA�A�A�E�A�G�A�G�A�G�A�G�A�M�A�VA�\)A�`BA�ffA�ffA�hsA�v�A�~�AҍPAҧ�A��A�(�A�~�A�bNA�7LA�bA���A�A��A�$�A�K�A�VA�^5A�ZA�VA�Q�A�9XA�&�A�  A��A���A�Aӝ�A���A���Ạ�A���A�VAƸRA�VA�K�A�ȴA��A��A�p�A���A��A�VA��RA�ffA�hsA��A��A�I�A�z�A��A���A���A��A�E�A���A��A��A�A�A�&�A��^A��A�/A�oA�1'A��^A�bNA�+A�^5A�Q�A��A��RA�&�A�A��A��A��HA�"�A���A�t�A�&�A��A��-A�/A�ȴA�ffA�n�A��hA�G�A�1'A{��Au�#AoS�AiAc��A\��AZ~�AX�HAWXAV�AV �AT��AQ��AP��ANQ�AK�TAKdZAJ9XAD��AC�AB�AB5?AA��AAA@~�A?hsA=ƨA=7LA:�`A9+A8�A7��A5�A4��A3�A0Q�A-C�A*v�A(�/A(�uA(ZA({A'��A'A%l�A �/AhsA�DA�
Al�A
=A��A�/A�DA5?A�A�+AZA��A�7A"�AA�`A��A��A�+A{A/A��A(�A�#A|�AoA�9A �AS�A1'Ax�A
��A
�A	�A�A~�A��A%AffA(�A�
A5?A�A�^AO�Av�A1A�A��AA�A ��A (�A -@��P@��@�o@���@���@��
@��@�v�@��@��D@��@��@���@��H@�p�@�9@�r�@��@���@��@웦@�33@��y@��@���@�hs@�u@�ƨ@�;d@���@�{@��/@�F@�dZ@���@�C�@�=q@�@�r�@߶F@ߍP@�dZ@�33@�"�@�"�@���@ޗ�@�E�@��@ݙ�@�p�@�%@�Z@�;d@�J@�hs@�&�@ج@�r�@��@׾w@�S�@ְ!@�V@���@թ�@��/@�  @��;@��;@Ӯ@�
=@�n�@�p�@��/@Ь@�1@�l�@��@���@�ȴ@�?}@�1'@�  @�S�@��y@���@ʟ�@�^5@�J@ɺ^@�@Ɂ@�?}@�V@ȓu@�I�@� �@�t�@���@�{@���@�A�@�|�@���@�v�@���@�G�@�7L@�?}@��`@��@��;@��@��@�l�@�\)@�S�@�\)@�\)@�dZ@�l�@�l�@�dZ@��@�n�@�E�@�E�@�5?@���@�p�@���@�z�@�I�@���@��F@�\)@�+@�@�ȴ@�v�@�-@��T@��^@���@�hs@�V@���@�z�@�1'@�dZ@��\@�-@���@���@�X@��@���@�I�@� �@���@��P@�;d@�+@�@�ȴ@��\@�=q@��@���@�hs@��/@�Q�@��@�|�@�\)@��@���@�M�@��h@�7L@��`@�j@��m@���@��w@��@��@�K�@���@�v�@�n�@�n�@�n�@�V@�E�@�$�@��@���@���@��7@�x�@�X@���@�r�@�(�@���@�ȴ@�=q@�@�hs@�/@���@��@���@�j@�  @��
@���@�S�@�
=@��@�5?@��@��-@�hs@�/@���@��j@��D@�bN@�1@��F@�dZ@�;d@��@��!@�v�@�=q@���@�@�X@�&�@�V@��`@��@�b@�  @��@�o@�M�@�=q@�{@��@��T@���@��-@�p�@���@��@�dZ@�@���@��R@���@�n�@�$�@��-@��@���@���@��u@�A�@��w@��@�l�@�%@�Z@��9@�1'@|9X@qhs@h��@`�9@V{@N��@F{@?l�@7�@2�@+��@%�T@l�@��@�@�@�+111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�-A�33A�33A�5?A�5?A�7LA�7LA�9XA�7LA�;dA�A�A�E�A�G�A�G�A�G�A�G�A�M�A�VA�\)A�`BA�ffA�ffA�hsA�v�A�~�AҍPAҧ�A��A�(�A�~�A�bNA�7LA�bA���A�A��A�$�A�K�A�VA�^5A�ZA�VA�Q�A�9XA�&�A�  A��A���A�Aӝ�A���A���Ạ�A���A�VAƸRA�VA�K�A�ȴA��A��A�p�A���A��A�VA��RA�ffA�hsA��A��A�I�A�z�A��A���A���A��A�E�A���A��A��A�A�A�&�A��^A��A�/A�oA�1'A��^A�bNA�+A�^5A�Q�A��A��RA�&�A�A��A��A��HA�"�A���A�t�A�&�A��A��-A�/A�ȴA�ffA�n�A��hA�G�A�1'A{��Au�#AoS�AiAc��A\��AZ~�AX�HAWXAV�AV �AT��AQ��AP��ANQ�AK�TAKdZAJ9XAD��AC�AB�AB5?AA��AAA@~�A?hsA=ƨA=7LA:�`A9+A8�A7��A5�A4��A3�A0Q�A-C�A*v�A(�/A(�uA(ZA({A'��A'A%l�A �/AhsA�DA�
Al�A
=A��A�/A�DA5?A�A�+AZA��A�7A"�AA�`A��A��A�+A{A/A��A(�A�#A|�AoA�9A �AS�A1'Ax�A
��A
�A	�A�A~�A��A%AffA(�A�
A5?A�A�^AO�Av�A1A�A��AA�A ��A (�A -@��P@��@�o@���@���@��
@��@�v�@��@��D@��@��@���@��H@�p�@�9@�r�@��@���@��@웦@�33@��y@��@���@�hs@�u@�ƨ@�;d@���@�{@��/@�F@�dZ@���@�C�@�=q@�@�r�@߶F@ߍP@�dZ@�33@�"�@�"�@���@ޗ�@�E�@��@ݙ�@�p�@�%@�Z@�;d@�J@�hs@�&�@ج@�r�@��@׾w@�S�@ְ!@�V@���@թ�@��/@�  @��;@��;@Ӯ@�
=@�n�@�p�@��/@Ь@�1@�l�@��@���@�ȴ@�?}@�1'@�  @�S�@��y@���@ʟ�@�^5@�J@ɺ^@�@Ɂ@�?}@�V@ȓu@�I�@� �@�t�@���@�{@���@�A�@�|�@���@�v�@���@�G�@�7L@�?}@��`@��@��;@��@��@�l�@�\)@�S�@�\)@�\)@�dZ@�l�@�l�@�dZ@��@�n�@�E�@�E�@�5?@���@�p�@���@�z�@�I�@���@��F@�\)@�+@�@�ȴ@�v�@�-@��T@��^@���@�hs@�V@���@�z�@�1'@�dZ@��\@�-@���@���@�X@��@���@�I�@� �@���@��P@�;d@�+@�@�ȴ@��\@�=q@��@���@�hs@��/@�Q�@��@�|�@�\)@��@���@�M�@��h@�7L@��`@�j@��m@���@��w@��@��@�K�@���@�v�@�n�@�n�@�n�@�V@�E�@�$�@��@���@���@��7@�x�@�X@���@�r�@�(�@���@�ȴ@�=q@�@�hs@�/@���@��@���@�j@�  @��
@���@�S�@�
=@��@�5?@��@��-@�hs@�/@���@��j@��D@�bN@�1@��F@�dZ@�;d@��@��!@�v�@�=q@���@�@�X@�&�@�V@��`@��@�b@�  @��@�o@�M�@�=q@�{@��@��T@���@��-@�p�@���@��@�dZ@�@���@��R@���@�n�@�$�@��-@��@���@���@��u@�A�@��w@��G�O�@�%@�Z@��9@�1'@|9X@qhs@h��@`�9@V{@N��@F{@?l�@7�@2�@+��@%�T@l�@��@�@�@�+111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�{B
�uB
�{B
�uB
�uB
�uB
�uB
�{B
�{B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�!B
�9B
�LB
�qB
ɺB
��Bt�B��B��B��B��B��B��B��B��B�-B�?B�LB�LB�FB�FB�3B�!B�B�B��B��B��B�RB��B�B�BB�TB��B�B�B"�B7LBP�BcTB��B��B��B��B��B��B��B��B��B��B�\B�DB�Bw�BbNB]/BVBO�Bm�Bw�Bs�Bo�Bp�Be`BR�BP�BK�BD�B7LB �BhB  B��B�B�HB��B�LB�B��B��B��B�JBjB2-B
=B
�B
�NB
��B
}�B
@�B	��B	ǮB	��B	o�B	J�B	&�B	�B	�B	bB	PB	1B	B��B�B�B�mB�ZB�;B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�B�)B�ZB�mB�B�B�B��B	B	B	B	VB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	'�B	)�B	-B	.B	.B	1'B	1'B	1'B	0!B	1'B	49B	7LB	6FB	9XB	;dB	:^B	9XB	8RB	:^B	B�B	B�B	@�B	@�B	A�B	@�B	B�B	B�B	D�B	C�B	E�B	I�B	N�B	M�B	O�B	XB	_;B	^5B	]/B	]/B	]/B	aHB	aHB	_;B	bNB	cTB	cTB	dZB	e`B	hsB	iyB	p�B	q�B	r�B	v�B	v�B	w�B	y�B	z�B	|�B	}�B	~�B	� B	�B	�B	�=B	�VB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�?B	�dB	�jB	�jB	�qB	�}B	�}B	�}B	��B	��B	��B	B	ÖB	ÖB	ÖB	ÖB	ŢB	ǮB	ƨB	ĜB	B	B	B	ÖB	ŢB	ǮB	ȴB	ɺB	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�
B	�B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�#B	�)B	�/B	�/B	�;B	�;B	�5B	�5B	�5B	�5B	�/B	�/B	�/B	�5B	�5B	�5B	�;B	�BB	�BB	�HB	�NB	�TB	�TB	�TB	�NB	�BB	�5B	�;B	�;B	�;B	�;B	�BB	�HB	�TB	�ZB	�ZB	�ZB	�ZB	�fB	�mB	�fB	�fB	�mB	�fB	�fB	�`B	�`B	�`B	�fB	�fB	�fB	�fB	�fB	�mB	�mB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
%B
+B
+B
1B

=B
bB
{B
�B
%�B
-B
-B
2-B
9XB
B�B
G�B
K�B
Q�B
XB
^5B
cTB
iyB
m�B
s�B
x�B
{�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
�eB
�^B
�dB
�^B
�^B
�^B
�`B
�bB
�dB
�qB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�
B
�"B
�5B
�XB
ɡB
��Bt�B��B��B��B��B�xB��B��B��B�B�!B�3B�/B�*B�,B�B�B��B��B��B��B��B�6B��B��B�$B�9B��BeB�B"�B7/BP�Bc5B��B��B��B��B��B��B��B��B��B�dB�?B�&B��Bw�Bb.B]BU�BO�BmuBw�Bs�Bo}Bp�Be=BR�BP�BK�BD~B7+B �BHB��B��B��B�'B�_B�,B��B��B��B��B�+Bj]B2B
B
�B
�,B
ιB
}�B
@eB	��B	ǒB	�oB	o�B	J�B	&�B	�B	iB	NB	<B	B	�B��B�B�B�UB�EB�&B�B��B��B��B��B��B��B��B̹B˳B˳B˱B˳B;B��B̺B��B��B��B�B�B�B� B� B� B��B��B�B�BB�UB�fB�lB�yB��B	 B	B	B	<B	lB	�B	}B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	'�B	)�B	,�B	-�B	-�B	1	B	1	B	1	B	0B	1B	4B	7/B	6)B	99B	;GB	:AB	9:B	85B	:CB	BqB	BsB	@gB	@fB	AlB	@fB	BsB	BpB	D~B	CyB	E�B	I�B	N�B	M�B	O�B	W�B	_B	^B	]B	]B	]B	a'B	a)B	_B	b/B	c6B	c5B	d9B	eAB	hTB	iZB	p�B	q�B	r�B	v�B	v�B	w�B	y�B	z�B	|�B	}�B	~�B	�B	��B	��B	�B	�6B	�^B	�eB	�\B	�aB	�cB	�lB	�xB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�AB	�IB	�GB	�NB	�YB	�YB	�YB	�^B	�`B	�eB	�lB	�sB	�rB	�pB	�qB	�}B	ǊB	ƃB	�yB	�iB	�jB	�lB	�sB	�~B	ǍB	ȏB	ɘB	ʝB	ˣB	ϺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�
B	�	B	�
B	�B	�B	�B	�B	�B	�B	�"B	�'B	�.B	�/B	�1B	�)B	�B	�B	�B	�B	�B	�B	�B	�"B	�/B	�5B	�3B	�5B	�4B	�@B	�HB	�@B	�BB	�GB	�AB	�AB	�<B	�:B	�>B	�@B	�@B	�@B	�AB	�AB	�FB	�GB	�GB	�HB	�MB	�TB	�XB	�XB	�YB	�WB	�XB	�bB	�_B	�_B	�bB	�_B	�_B	�_B	�`B	�_B	�_B	�`B	�gB	�fB	�_B	�eB	�fB	�aB	�gB	�fB	�cB	�_B	�`B	�aB	�^B	�aB	�dB	�^B	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 B
�B
�B
B
G�O�B

B
<B
RB
]B
%�B
,�B
,�B
2B
90B
BgB
G�B
K�B
Q�B
W�B
^B
c+B
iSB
mjB
s�B
x�B
{�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.35 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451132016080714511320160807145113  AO  ARCAADJP                                                                    20150226221426    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221426  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221426  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145113  IP                  G�O�G�O�G�O�                