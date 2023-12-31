CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:31Z UW 3.1 conversion   
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130142434  20190522121827  1727_5046_144                   2C  D   APEX                            2143                            040306                          846 @���Q)_�1   @����x@6�dZ��c�t�j~�1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C �C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Ca�fCd  Cf  Ch  Cj  Cl�Cn�Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1�fD2  D2� D3  D3� D4  D4� D5  D5� D5��D6� D7  D7� D8  D8� D9  D9�fD:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DAy�DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQfDQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dc��Ddy�De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dq��Dr� Ds  Ds� Dy�fD�6fD�l�D�� D���D�  D�ffD���D��D� D�\�D���D��3D�&fD�I�DڦfD��3D�)�D�` D�D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���@�33A��A!��AA��Aa��A���A���A���A���A���A���A���A���B ffBffBffBffB ffB(ffB0ffB8ffB@ffBHffBPffBXffB`ffBhffBpffBxffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�ffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C 33C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb  Cd�Cf�Ch�Cj�Cl33Cn33Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D fD �fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD	fD	�fD
fD
�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD fD �fD!fD!�fD"fD"�fD#fD#�fD$fD$�fD%fD%�fD&fD&�fD'fD'�fD(fD(�fD)fD)�fD*fD*�fD+fD+�fD,fD,�fD-fD-�fD.fD.�fD/fD/�fD0fD0�fD1fD1��D2fD2�fD3fD3�fD4fD4�fD5fD5�fD6  D6�fD7fD7�fD8fD8�fD9fD9��D:fD:�fD;fD;�fD<fD<�fD=fD=�fD>fD>�fD?fD?�fD@fD@�fDAfDA� DBfDB�fDCfDC�fDDfDD�fDEfDE�fDFfDF�fDGfDG�fDHfDH�fDIfDI�fDJfDJ�fDKfDK�fDLfDL�fDMfDM�fDNfDN�fDOfDO�fDPfDP�fDQ�DQ�fDRfDR�fDSfDS�fDTfDT�fDUfDU�fDVfDV�fDWfDW�fDXfDX�fDYfDY�fDZfDZ�fD[fD[�fD\fD\�fD]fD]�fD^fD^�fD_fD_�fD`fD`�fDafDa�fDbfDb�fDcfDc�fDd  Dd� DefDe�fDffDf�fDgfDg�fDhfDh�fDifDi�fDjfDj�fDkfDk�fDlfDl�fDmfDm�fDnfDn�fDofDo�fDpfDp�fDqfDq�fDr  Dr�fDsfDs�fDy��D�9�D�p D��3D�� D�#3D�i�D�� D���D�3D�` D�� D��fD�)�D�L�Dک�D��fD�,�D�c3D��D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�|�A�~�A�v�A�dZA�A�A�-A�
=A�A�bA��mA��A�dZA�^5A�Q�A�9XA�&�A��A�oA�1A�A�  A��A���A�S�A��PA���A��PA�z�A�hsA�dZA�p�A�"�A���A��^A���A�S�A�?}A�1'A�&�A�bA���A���A�`BA�33A�-A�-A�bA��^A���A�(�A�bA��HA�5?A���A��7A�Q�A���A�  A���A�r�A���A�?}A��A��9A��A�M�A���A�A�A�M�A��#A� �A�S�A���A�~�A��!A��!A��uA��;A�9XA�
=A��A��+A�Q�A�bNA��^A���A�oA���A��+A�=qA��!A���A��+A���A���A�n�A��A���A��9A��!A�G�A�bA��yA�"�A�v�A�~�A�M�A���A�7LA���A�  A��RA���A��wA��A�dZA�A���A��A�{A}��Az�yAy��Ax��AwK�Av1Au�^At�As��Ar��Aq�Ap{An��Am�Am+AlĜAl �Ak
=Ait�Ah�RAg%Ad�Ac�7Ab��Ab  Aa?}A`VA_%A]�A]+A\1A[�A[%AZ��AZ(�AX�HAW|�AU��AT{ARjAQAQ��APZAO|�AO+AN�AN$�AM��AL��AL�AKoAI�AHjAGp�AE�7AChsAB��ABbNAB-AA��AA��AAp�A@ZA?�wA?`BA=p�A;��A97LA5`BA2�A17LA0I�A/+A,�jA+p�A*M�A)dZA(��A'�hA&Q�A%��A%hsA$-A"bNA!�hA ��A   AoA�+A�A33A��A�A�jAE�AK�A�A��AE�A�-A�A��A�+AQ�A�hAbNAVA��AAJA
��A	��A��A�;AO�A1A�!A�A�`A~�AE�A�A�wA&�A bN@���@�ȴ@���@���@�\)@��/@���@��@�
=@�ȴ@�@�+@�@���@�@�+@�R@�9X@�$�@��T@�@�O�@�@��@��#@�t�@�{@���@�^5@�b@���@��@�ȴ@ӥ�@�V@��T@�x�@���@�I�@��m@�|�@�+@ΰ!@��T@̛�@�dZ@�5?@�&�@�Z@�o@�hs@��@��@�  @�S�@�v�@�X@�1'@�|�@��@�z�@���@�|�@�33@���@�~�@�{@��@���@��@�5?@���@��j@��m@���@�|�@�dZ@�o@���@��^@�%@�?}@�j@��F@�@���@���@��^@�hs@�7L@�9X@���@�K�@�o@��R@�=q@�J@��T@���@���@��u@��
@�dZ@�33@��H@�~�@�V@�-@��@��@��h@�/@�Ĝ@�z�@��@�t�@�33@��@���@�$�@���@�&�@���@��D@��@�ƨ@�(�@��@��@�9X@��@�ƨ@�\)@��@���@�$�@��@�&�@�&�@��@��@���@���@�ƨ@�K�@�S�@�C�@�@�{@��`@��9@�r�@� �@��@��m@���@��w@��F@��@��@���@�O�@���@�(�@�t�@��@��@�x�@�V@�%@���@���@��/@���@�r�@��@�@�
=@�o@�o@���@�ȴ@���@�V@�5?@��@���@�p�@��7@�?}@�7L@�p�@�p�@�G�@���@�Ĝ@��@��@�t�@���@�@��!@�ff@�-@�5?@�@�@�J@��@�/@���@��@��@�Q�@�1'@�b@�1@��@�r�@�9X@���@��;@�ƨ@���@�t�@�C�@��@�ȴ@�^5@�{@���@�p�@���@�9X@��m@��;@��;@��;@���@��P@�|�@���@|�D@p�`@hĜ@\1@Tz�@L��@Gl�@Co@;�@6v�@.ȴ@(b@#��@l�@�^@��@x�@z�@�u@O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�|�A�~�A�v�A�dZA�A�A�-A�
=A�A�bA��mA��A�dZA�^5A�Q�A�9XA�&�A��A�oA�1A�A�  A��A���A�S�A��PA���A��PA�z�A�hsA�dZA�p�A�"�A���A��^A���A�S�A�?}A�1'A�&�A�bA���A���A�`BA�33A�-A�-A�bA��^A���A�(�A�bA��HA�5?A���A��7A�Q�A���A�  A���A�r�A���A�?}A��A��9A��A�M�A���A�A�A�M�A��#A� �A�S�A���A�~�A��!A��!A��uA��;A�9XA�
=A��A��+A�Q�A�bNA��^A���A�oA���A��+A�=qA��!A���A��+A���A���A�n�A��A���A��9A��!A�G�A�bA��yA�"�A�v�A�~�A�M�A���A�7LA���A�  A��RA���A��wA��A�dZA�A���A��A�{A}��Az�yAy��Ax��AwK�Av1Au�^At�As��Ar��Aq�Ap{An��Am�Am+AlĜAl �Ak
=Ait�Ah�RAg%Ad�Ac�7Ab��Ab  Aa?}A`VA_%A]�A]+A\1A[�A[%AZ��AZ(�AX�HAW|�AU��AT{ARjAQAQ��APZAO|�AO+AN�AN$�AM��AL��AL�AKoAI�AHjAGp�AE�7AChsAB��ABbNAB-AA��AA��AAp�A@ZA?�wA?`BA=p�A;��A97LA5`BA2�A17LA0I�A/+A,�jA+p�A*M�A)dZA(��A'�hA&Q�A%��A%hsA$-A"bNA!�hA ��A   AoA�+A�A33A��A�A�jAE�AK�A�A��AE�A�-A�A��A�+AQ�A�hAbNAVA��AAJA
��A	��A��A�;AO�A1A�!A�A�`A~�AE�A�A�wA&�A bN@���@�ȴ@���@���@�\)@��/@���@��@�
=@�ȴ@�@�+@�@���@�@�+@�R@�9X@�$�@��T@�@�O�@�@��@��#@�t�@�{@���@�^5@�b@���@��@�ȴ@ӥ�@�V@��T@�x�@���@�I�@��m@�|�@�+@ΰ!@��T@̛�@�dZ@�5?@�&�@�Z@�o@�hs@��@��@�  @�S�@�v�@�X@�1'@�|�@��@�z�@���@�|�@�33@���@�~�@�{@��@���@��@�5?@���@��j@��m@���@�|�@�dZ@�o@���@��^@�%@�?}@�j@��F@�@���@���@��^@�hs@�7L@�9X@���@�K�@�o@��R@�=q@�J@��T@���@���@��u@��
@�dZ@�33@��H@�~�@�V@�-@��@��@��h@�/@�Ĝ@�z�@��@�t�@�33@��@���@�$�@���@�&�@���@��D@��@�ƨ@�(�@��@��@�9X@��@�ƨ@�\)@��@���@�$�@��@�&�@�&�@��@��@���@���@�ƨ@�K�@�S�@�C�@�@�{@��`@��9@�r�@� �@��@��m@���@��w@��F@��@��@���@�O�@���@�(�@�t�@��@��@�x�@�V@�%@���@���@��/@���@�r�@��@�@�
=@�o@�o@���@�ȴ@���@�V@�5?@��@���@�p�@��7@�?}@�7L@�p�@�p�@�G�@���@�Ĝ@��@��@�t�@���@�@��!@�ff@�-@�5?@�@�@�J@��@�/@���@��@��@�Q�@�1'@�b@�1@��@�r�@�9X@���@��;@�ƨ@���@�t�@�C�@��@�ȴ@�^5@�{@���@�p�@���@�9X@��m@��;@��;@��;@���@��P@�|�@���@|�D@p�`@hĜ@\1@Tz�@L��@Gl�@Co@;�@6v�@.ȴ@(b@#��@l�@�^@��@x�@z�@�u@O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�VB�VB�\B�bB�uB��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B��B��B��B�B�B�B�B�B�3BƨBǮBƨBŢBɺB��B��B��B��B��B��B��B��B��B��B�
B�B�)B�fB�mB�B��BhB�B�B#�B0!B33B9XB:^B;dBA�B>wB2-B2-B33B:^B)�BB��B�dB�'B��B��B�VB�By�Bx�B|�B�B�Bw�BhsB^5BK�B,B�BhBVB%B��B�B�-B�PBn�BZBI�B1'B"�B�B�B	7B
��B
�B
�5B
��B
ȴB
B
�jB
�-B
��B
��B
��B
��B
�{B
�bB
�=B
�B
v�B
jB
ZB
R�B
M�B
F�B
A�B
@�B
;dB
8RB
0!B
)�B
!�B
�B
�B
oB
bB
JB
+B
  B	��B	�B	�sB	�NB	�/B	�B	��B	��B	ǮB	B	�qB	�RB	�?B	�-B	�!B	�B	��B	��B	��B	�DB	�B	~�B	�B	x�B	r�B	o�B	k�B	gmB	dZB	_;B	YB	P�B	F�B	A�B	:^B	.B	�B	&�B	+B	-B	1'B	49B	33B	-B	)�B	$�B	�B	1B�B��BÖB�wB�dB�FB�!B�B��B��B��B��B��B��B��B�uB�bB�VB�PB�DB�DB�=B�7B�1B�B�B�B� B~�B~�B~�B|�Bz�Bx�Bx�Bx�Bw�Bu�Bq�Bm�Bo�Bo�Bn�Bl�BjBhsBffBcTB`BB^5B\)B[#BZBYBYBW
BT�BS�BR�BQ�BP�BN�BM�BK�BJ�BJ�BJ�BJ�BJ�BI�BH�BG�BG�BF�BD�BE�BE�BE�BD�BD�BC�BB�BA�BA�BA�B@�BC�BH�BI�BI�BJ�BN�BP�BQ�BR�BR�BS�BT�BT�BT�BVBVBXBZBZB[#BZBZB[#B_;B`BBdZBffBgmBjBm�Bl�Bt�Bu�Bw�Bx�By�Bz�Bz�B|�B|�B� B�B�1B�VB�VB�bB�oB�uB�{B�uB�uB��B��B��B��B��B��B��B�B�B�B�B�B�!B�'B�!B�'B�-B�3B�3B�9B�LB�^B�dB�qB��BƨB��B��B��B��B��B��B��B�B�B�B�/B�5B�HB�TB�fB�B�B�B�B��B��B	B	1B	DB	DB	VB	bB	hB	bB	oB	�B	�B	�B	�B	�B	 �B	!�B	"�B	&�B	'�B	(�B	+B	)�B	/B	49B	6FB	7LB	7LB	8RB	<jB	<jB	=qB	=qB	A�B	G�B	YB	]/B	]/B	\)B	[#B	YB	YB	]/B	gmB	iyB	k�B	l�B	m�B	m�B	u�B	|�B	~�B	�B	�B	�B	�B	�B	�%B	�1B	�=B	�PB	�VB	�VB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�-B	�3B	�9B	�?B	�dB	�jB	�qB	�}B	�}B	�}B	�}B	��B	ŢB	ǮB	ɺB	��B	��B	ɺB	ɺB	ɺB	ɺB	ɺB	ɺB	ɺB	ɺB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�;B	�B	��B
JB
�B
 �B
.B
5?B
8RB
>wB
D�B
I�B
Q�B
W
B
ZB
aHB
ffB
jB
o�B
t�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�VB�VB�\B�hB�uB��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B��B�B�B�B�B�B�9BǮBǮBǮBƨBɺB��B��B��B��B��B��B��B��B��B��B�B�#B�5B�fB�mB�B��BoB�B�B%�B1'B49B<jB<jB<jBB�BB�B5?B5?B5?B?}B1'B
=B��B�qB�9B�B��B�{B�%B}�By�B}�B�B�7B}�Bl�BcTBXB7LB�BoBbB
=B  B�TB�jB��Bu�B`BBR�B6FB$�B�B�BPBB
��B
�`B
��B
��B
ŢB
��B
�^B
��B
��B
��B
��B
��B
�oB
�VB
�B
{�B
q�B
]/B
T�B
Q�B
I�B
B�B
B�B
=qB
;dB
33B
.B
$�B
�B
�B
uB
oB
\B
DB
B
  B	��B	�B	�`B	�;B	�#B	�
B	��B	��B	ĜB	��B	�XB	�FB	�3B	�-B	�'B	�B	��B	��B	�bB	�B	� B	�B	{�B	s�B	p�B	m�B	iyB	ffB	bNB	]/B	VB	J�B	D�B	@�B	5?B	�B	(�B	,B	.B	1'B	6FB	7LB	/B	,B	+B	�B	bB��B�)BǮB��B�wB�jB�3B�B��B��B��B��B��B��B��B��B�oB�bB�bB�VB�PB�JB�DB�=B�DB�1B�B�B� B� B� B~�B� B|�By�By�By�Bx�Bu�Bq�Bq�Br�Bq�Bn�Bl�BjBgmBffBcTB`BB^5B\)B[#BZBZBYBW
BVBS�BS�BR�BP�BQ�BO�BN�BM�BK�BJ�BJ�BJ�BI�BI�BI�BG�BH�BH�BF�BE�BE�BE�BE�BF�BE�BC�BC�BD�BG�BJ�BL�BM�BO�BP�BQ�BR�BS�BS�BT�BVBVBVBW
BXBZB\)B\)B\)B\)B]/B^5BaHBbNBe`BgmBhsBk�Bn�Bo�Bu�Bv�Bx�Bx�Bz�B{�B{�B}�B}�B�B�B�=B�\B�\B�hB�oB�uB��B�{B��B��B��B��B��B��B��B��B�B�B�B�B�!B�'B�-B�'B�-B�-B�3B�9B�?B�RB�dB�jB�wBBǮB��B��B��B��B��B��B��B�B�#B�#B�5B�;B�HB�ZB�mB�B�B�B�B��B��B	B	1B	DB	DB	\B	hB	hB	hB	uB	�B	�B	�B	�B	�B	 �B	"�B	#�B	'�B	'�B	(�B	,B	,B	1'B	49B	6FB	8RB	7LB	8RB	<jB	<jB	=qB	=qB	A�B	F�B	XB	^5B	^5B	]/B	\)B	[#B	ZB	^5B	gmB	iyB	k�B	l�B	m�B	n�B	v�B	}�B	~�B	�B	�B	�B	�B	�B	�+B	�1B	�DB	�VB	�VB	�VB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�-B	�3B	�9B	�FB	�dB	�jB	�qB	�}B	�}B	�}B	�}B	��B	ŢB	ǮB	ɺB	��B	��B	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	��B	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�;B	�B	��B
JB
�B
 �B
.B
5?B
8RB
>wB
D�B
I�B
Q�B
W
B
ZB
`BB
ffB
jB
o�B
t�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.1 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447242012010314472420120103144724  AO  ARGQ                                                                        20111130142434  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130142434  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144724  IP                  G�O�G�O�G�O�                