CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-04-15T02:16:50Z AOML 3.0 creation; 2016-08-07T21:36:45Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20160415021650  20160807143645  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               pA   AO  5286_8897_112                   2C  D   APEX                            6531                            072314                          846 @פ�>cwD1   @פ�βZO@3���S���cK�vȴ91   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    pA   B   B   @�  @�  A   A   A@  A`  A�  A���A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�C�fC�fC
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� DtfDt� Dt�fDy��D�3D�L�D��fD�� D���D�L�D�vfD���D��D�C3D�s3D��3D�fD�6fD�ffD���D�3D�0 D�3D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�33@�33A��A!��AA��Aa��A���A���A���A���A���A���A���A���B ffBffBffBffB ffB(ffB0ffB8ffB@ffBHffBPffBXffB`ffBhffBpffBxffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C �C�C34C  C  C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D fD �fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD	fD	�fD
fD
�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD fD �fD!fD!�fD"fD"�fD#fD#�fD$fD$�fD%fD%�fD&fD&�fD'fD'�fD(fD(�fD)fD)�fD*fD*�fD+fD+�fD,fD,�fD-fD-�fD.fD.�fD/fD/�fD0fD0�fD1fD1�fD2fD2�fD3fD3�fD4fD4�fD5fD5�fD6fD6�fD7fD7�fD8fD8�fD9fD9�fD:fD:�fD;fD;�fD<fD<�fD=fD=�fD>fD>�fD?fD?�fD@fD@�fDAfDA�fDBfDB�fDCfDC�fDDfDD�fDEfDE�fDFfDF�fDGfDG�fDHfDH�fDIfDI�fDJfDJ�fDKfDK�fDLfDL�fDMfDM�fDNfDN�fDOfDO�fDPfDP�fDQfDQ�fDRfDR�fDSfDS�fDTfDT�fDUfDU�fDVfDV�fDWfDW�fDXfDX�fDYfDY�fDZfDZ�fD[fD[�fD\fD\�fD]fD]�fD^fD^�fD_fD_�fD`fD`�fDafDa�fDbfDb�fDcfDc�fDdfDd�fDefDe�fDffDf�fDgfDg�fDhfDh�fDifDi�fDjfDj�fDkfDk�fDlfDl�fDmfDm�fDnfDn�fDofDo�fDpfDp�fDqfDq�fDrfDr�fDsfDs�fDt�Dt�fDt��Dy�3D�fD�P D���D��3D���D�P D�y�D�� D��D�FfD�vfD��fD��D�9�D�i�D�� D�fD�33D�fD��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�^5A�^5A�^5A�`BA�`BA�bNA�bNA�bNA�bNA�bNA�bNA�dZA�dZA�ffA�ffA�hsA�hsA�jA�l�A�jA�jA�ffA�\)A�oAɶFAɟ�A�Aə�A�|�A�M�A�&�A��A�bNAǝ�A�
=A�ȴAƲ-AƅA��HA�(�A�5?Aú^A��;A�n�A��A��A�K�A��RA��^A�ȴA��hA�r�A�
=A���A���A�A�^5A��-A��!A���A�oA��A��A�bA�A�A�+A���A�ƨA�bNA�&�A�O�A�~�A��A�33A��9A���A�S�A�
=A�S�A��jA�n�A�M�A�VA�5?A���A�/A�ȴA��A�VA�S�A|�9Ay�Ax�AvjAq�hAo�Aj��Agx�Af�9Ae|�AbVA`A�A^�DAY�AWt�AUVATffAS��APȴAL�AJ~�AIVAG��AG
=AFĜAEhsAC�ACAA�A>��A=C�A<��A<=qA;�A:�/A9dZA7�;A6Q�A3;dA1�A/t�A-dZA++A)�-A)G�A)oA( �A%�#A$�A#`BA" �A �yA �DA 1'AhsAbA�wA�PA��A�
AA��A1'A�7A
=A�+A{A�A�7A�Av�AVA(�A�-A�/AbA��A�#A"�AbNAA�A(�A�mAx�AS�A�A�A;dAn�A  AA	�FAZAA�-A��Ax�A��AE�A��A�!Al�A�!A�7@��@�33@���@�ff@�-@�V@���@�Q�@��@���@�@��
@�(�@�D@���@���@�v�@�+@��F@��D@�r�@��@�Ĝ@��`@��@�1'@���@���@�h@��@�bN@��@�+@�`B@�Z@�P@���@�V@�%@֧�@�v�@���@�
=@�?}@�/@Ϯ@�r�@��@˥�@�t�@��@ə�@ɉ7@�b@�$�@Ƨ�@��y@�dZ@�33@�M�@�5?@�$�@��@�$�@�-@�{@���@�G�@�I�@��@�~�@�V@�-@��-@�@���@�A�@��@��@��\@�ff@�=q@�^5@�v�@�v�@��\@���@���@���@�O�@�(�@�\)@�o@�V@��h@���@��9@��D@�1'@��m@�o@��@�;d@�;d@�;d@��R@��@��7@��j@�r�@�Z@�9X@�(�@�1@�ƨ@�l�@�@���@���@��R@��R@���@�n�@�n�@�^5@�=q@�5?@�J@��T@��#@��@�{@���@��#@�@���@�p�@�/@���@�bN@�(�@�ƨ@�C�@�o@���@�5?@��7@��@�A�@��;@��@���@�@��T@��9@��@���@��
@��m@���@��P@���@��-@��^@���@�~�@��@�A�@�z�@�z�@�Z@�1'@���@��F@��@�+@��@��!@�~�@��T@�x�@��@�b@��w@��@���@��@�dZ@�"�@�ȴ@���@�v�@�V@���@���@��7@�p�@��@�Q�@��@�t�@�l�@�dZ@�\)@��y@��@�`B@���@���@��@��\@�v�@�n�@�n�@�ff@���@�hs@�/@��@��@��m@��F@���@�K�@�
=@��H@��\@��#@�X@��`@�z�@�A�@� �@��F@�\)@�"�@���@�J@��@��@��#@�@��^@��@�7L@���@���@��
@�ƨ@��F@���@���@�S�@�ȴ@��\@�n�@�=q@��@���@��@�@��h@�X@���@��`@�j@��@��F@��@���@��@�t�@�K�@�"�@��y@���@��\@�=q@�{@��#@���@��7@�p�@�`B@���@�j@yG�@q��@h��@`r�@Y�^@Q�^@HĜ@E�h@>ff@6��@/|�@'�;@#��@!��@��@Ĝ@^5@  @p�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�^5A�^5A�^5A�`BA�`BA�bNA�bNA�bNA�bNA�bNA�bNA�dZA�dZA�ffA�ffA�hsA�hsA�jA�l�A�jA�jA�ffA�\)A�oAɶFAɟ�A�Aə�A�|�A�M�A�&�A��A�bNAǝ�A�
=A�ȴAƲ-AƅA��HA�(�A�5?Aú^A��;A�n�A��A��A�K�A��RA��^A�ȴA��hA�r�A�
=A���A���A�A�^5A��-A��!A���A�oA��A��A�bA�A�A�+A���A�ƨA�bNA�&�A�O�A�~�A��A�33A��9A���A�S�A�
=A�S�A��jA�n�A�M�A�VA�5?A���A�/A�ȴA��A�VA�S�A|�9Ay�Ax�AvjAq�hAo�Aj��Agx�Af�9Ae|�AbVA`A�A^�DAY�AWt�AUVATffAS��APȴAL�AJ~�AIVAG��AG
=AFĜAEhsAC�ACAA�A>��A=C�A<��A<=qA;�A:�/A9dZA7�;A6Q�A3;dA1�A/t�A-dZA++A)�-A)G�A)oA( �A%�#A$�A#`BA" �A �yA �DA 1'AhsAbA�wA�PA��A�
AA��A1'A�7A
=A�+A{A�A�7A�Av�AVA(�A�-A�/AbA��A�#A"�AbNAA�A(�A�mAx�AS�A�A�A;dAn�A  AA	�FAZAA�-A��Ax�A��AE�A��A�!Al�A�!A�7@��@�33@���@�ff@�-@�V@���@�Q�@��@���@�@��
@�(�@�D@���@���@�v�@�+@��F@��D@�r�@��@�Ĝ@��`@��@�1'@���@���@�h@��@�bN@��@�+@�`B@�Z@�P@���@�V@�%@֧�@�v�@���@�
=@�?}@�/@Ϯ@�r�@��@˥�@�t�@��@ə�@ɉ7@�b@�$�@Ƨ�@��y@�dZ@�33@�M�@�5?@�$�@��@�$�@�-@�{@���@�G�@�I�@��@�~�@�V@�-@��-@�@���@�A�@��@��@��\@�ff@�=q@�^5@�v�@�v�@��\@���@���@���@�O�@�(�@�\)@�o@�V@��h@���@��9@��D@�1'@��m@�o@��@�;d@�;d@�;d@��R@��@��7@��j@�r�@�Z@�9X@�(�@�1@�ƨ@�l�@�@���@���@��R@��R@���@�n�@�n�@�^5@�=q@�5?@�J@��T@��#@��@�{@���@��#@�@���@�p�@�/@���@�bN@�(�@�ƨ@�C�@�o@���@�5?@��7@��@�A�@��;@��@���@�@��T@��9@��@���@��
@��m@���@��P@���@��-@��^@���@�~�@��@�A�@�z�@�z�@�Z@�1'@���@��F@��@�+@��@��!@�~�@��T@�x�@��@�b@��w@��@���@��@�dZ@�"�@�ȴ@���@�v�@�V@���@���@��7@�p�@��@�Q�@��@�t�@�l�@�dZ@�\)@��y@��@�`B@���@���@��@��\@�v�@�n�@�n�@�ff@���@�hs@�/@��@��@��m@��F@���@�K�@�
=@��H@��\@��#@�X@��`@�z�@�A�@� �@��F@�\)@�"�@���@�J@��@��@��#@�@��^@��@�7L@���@���@��
@�ƨ@��F@���@���@�S�@�ȴ@��\@�n�@�=q@��@���@��@�@��h@�X@���@��`@�j@��@��F@��@���@��@�t�@�K�@�"�@��y@���@��\@�=q@�{@��#@���@��7@�p�G�O�@���@�j@yG�@q��@h��@`r�@Y�^@Q�^@HĜ@E�h@>ff@6��@/|�@'�;@#��@!��@��@Ĝ@^5@  @p�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	\B	\B	bB	\B	\B	\B	\B	\B	\B	bB	\B	bB	bB	bB	bB	bB	bB	bB	bB	bB	bB	bB	bB	bB	bB	bB	bB	bB	bB	bB	hB	�B	�B	&�B	�B
oB
<jB
\)B
q�B
p�B
q�B
|�B
|�B
|�B
{�B
{�B
�B
�B
�{B
ÖB
��B
�BB�B9XBP�BW
BffB�B�uB��B��B�B�B�
BÖB�qB�B��B�B�ZB�mB�BBǮB��B�bB�B��B�sB��B�sB�3B�PB�+Be`BR�BE�B49BB
�yB
ƨB
�B
��B
�PB
�1B
�B
e`B
8RB
�B	��B	�B	�3B	��B	��B	�%B	k�B	[#B	E�B	5?B	1'B	,B	!�B	�B	bB	B��B�B�B�fB�)B��B��B��B��BɺBǮBŢBÖB��B�wB�dB�XB�XB�XB�LB�?B�-B�!B�B��B��B��B��B�{B��B��B��B��B�bB�DB�%B�B�B�B�B�B~�B}�B}�B{�By�B{�B|�B~�B�B�B�B�B�B�B�B�%B�%B�%B�%B�7B�+B�B�=B�JB�\B�\B�\B�\B�VB�VB�JB�DB�=B�PB��B��B��B�-B�-B�3B�9B�3B�-B�'B�!B�!B�'B�'B�B�!B�B�B�!B�-B�9B�!B�B�B�B�!B�3B�LB�jB�}B��B�BB�sB�B��B��B		7B	 �B	&�B	+B	.B	/B	%�B	�B	uB	VB	
=B	%B	B��B��B�B�TB�5B�)B�)B�/B�5B�#B�;B�BB�5B�5B�5B�;B�5B�/B�)B�B�B�BB�ZB�yB�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B	B	�B	&�B	"�B	!�B	 �B	!�B	"�B	$�B	,B	-B	.B	0!B	1'B	33B	1'B	/B	+B	)�B	)�B	(�B	(�B	+B	+B	+B	+B	)�B	.B	5?B	8RB	=qB	A�B	B�B	B�B	B�B	H�B	K�B	K�B	K�B	L�B	L�B	M�B	N�B	Q�B	S�B	S�B	T�B	T�B	VB	XB	XB	XB	ZB	aHB	e`B	gmB	iyB	k�B	q�B	s�B	s�B	u�B	|�B	�B	�7B	�=B	�PB	�VB	�\B	�hB	�hB	�bB	�\B	�VB	�oB	�uB	�{B	�{B	�{B	�{B	�{B	�{B	�{B	��B	��B	��B	��B	�PB	�=B	�DB	�DB	�PB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�-B	�-B	�-B	�-B	�-B	�-B	�3B	�3B	�9B	�?B	�?B	�?B	�FB	�XB	�jB	�qB	�qB	�wB	�wB	��B	ĜB	ĜB	ŢB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�/B	�;B	�BB	�HB	�HB	�HB	�ZB	�fB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
1B
\B
�B
�B
&�B
-B
49B
=qB
@�B
F�B
M�B
R�B
[#B
_;B
`BB
ffB
iyB
o�B
r�B
s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B	eB	hB	mB	hB	fB	hB	fB	jB	eB	oB	fB	oB	mB	lB	mB	nB	nB	mB	lB	nB	mB	lB	oB	nB	nB	mB	nB	mB	kB	kB	vB	�B	�B	&�B	�B
mB
<jB
\(B
q�B
p�B
q�B
|�B
|�B
|�B
{�B
{�B
�B
�B
�zB
ÏB
��B
�BB�B9OBP�BWBf_B�B�oB��B�zB�B�B� BÍB�iB�B��B�B�RB�cB�:BǥB��B�YB�B��B�hB��B�nB�(B�CB�$BeXBR�BE�B4/BB
�qB
ƠB
�B
��B
�KB
�)B
�B
e]B
8OB
�B	��B	�B	�1B	��B	��B	�'B	k�B	[&B	E�B	5EB	1+B	,B	!�B	�B	hB	 B��B�B�B�mB�1B��B��B��B��B��BǵBŬBÞB��B�B�kB�`B�cB�`B�UB�HB�5B�*B�B��B��B��B��B��B��B��B��B��B�lB�MB�/B�B�B�B�B�BB}�B}�B{�By�B{�B|�BB�B�B�"B�B�B�B�*B�.B�/B�.B�/B�BB�8B�(B�FB�TB�fB�fB�gB�fB�`B�`B�WB�NB�GB�XB��B��B��B�4B�4B�8B�>B�9B�4B�0B�(B�*B�-B�-B�%B�(B�B�B�'B�0B�AB�'B�B�B�#B�(B�8B�SB�oB��B��B�HB�xB�B��B��B		9B	 �B	&�B	+B	.B	/B	%�B	�B	vB	ZB	
=B	)B	B��B��B�B�YB�;B�.B�-B�3B�:B�'B�@B�EB�;B�;B�;B�?B�;B�2B�/B�B�!B�FB�^B�|B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B	B	�B	&�B	"�B	!�B	 �B	!�B	"�B	$�B	,	B	-B	.B	0#B	1%B	34B	1)B	/B	+B	)�B	)�B	(�B	(�B	+ B	+B	+B	+B	)�B	.B	5>B	8QB	=rB	A�B	B�B	B�B	B�B	H�B	K�B	K�B	K�B	L�B	L�B	M�B	N�B	Q�B	S�B	S�B	T�B	T�B	VB	XB	XB	XB	ZB	aGB	e^B	gjB	ixB	k�B	q�B	s�B	s�B	u�B	|�B	�B	�4B	�;B	�NB	�SB	�[B	�eB	�fB	�`B	�YB	�UB	�kB	�rB	�yB	�yB	�yB	�zB	�zB	�vB	�yB	��B	��B	��B	�zB	�LB	�9B	�@B	�@B	�MB	�lB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�
B	�B	�B	� B	�#B	�(B	�(B	�)B	�(B	�&B	�'B	�.B	�1B	�5B	�7B	�8B	�8B	�DB	�RB	�gB	�lB	�nB	�tB	�qB	��B	ĚB	ėB	şB	ȰB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�)B	�5B	�=B	�CB	�BB	�BB	�TB	�_B	�cB	�nB	�B	�B	�B	�B	�zB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B
B
)B
VB
�B
�B
&�B
-B
40B
=jB
@{B
F�B
M�B
R�B
[B
_1B
`;B
f_B
ipB
o�B
r�B
s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.1 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436452016080714364520160807143645  AO  ARCAADJP                                                                    20160415021650    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160415021650  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160415021650  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143645  IP                  G�O�G�O�G�O�                