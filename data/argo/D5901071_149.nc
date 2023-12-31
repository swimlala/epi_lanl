CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:32Z UW 3.1 conversion   
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
_FillValue                 �  A8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  T8   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ]�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  eP   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  g8   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  n�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  vh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  xP   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �h   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �$   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �(   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �8   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �<   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �@   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130142540  20190522121827  1727_5046_149                   2C  D   APEX                            2143                            040306                          846 @��=Eg��1   @��=�$ @66�+J�c�KƧ�1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@���@���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffB  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C#�fC&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF�CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb�Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,y�D-  D-� D.  D.� D/  D/�fD0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DQ��DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DYy�DY��DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dg��Dh� Di  Di�fDj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dpy�Dq  Dq� Dr  Dr� Ds  Dz3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�  @�  A   A!��AA��Aa��A���A���A���A���A���A���A���A���B ffB��BffBffB ffB(ffB0ffB8ffB@ffBHffBPffBXffB`ffBhffBpffBxffB�33B�33B�33B�33B�33B�  B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$  C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF33CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb33Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D fD �fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD	fD	�fD
fD
�fDfD�fD�D�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD��DfD�fD fD �fD!fD!�fD"fD"�fD#fD#�fD$fD$�fD%fD%�fD&fD&�fD'fD'�fD(fD(�fD)fD)�fD*fD*�fD+fD+�fD,fD,� D-fD-�fD.fD.�fD/fD/��D0fD0�fD1fD1�fD2fD2�fD3fD3�fD4fD4�fD5fD5�fD6fD6�fD7fD7�fD8fD8�fD9fD9�fD:fD:�fD;fD;�fD<fD<�fD=fD=�fD>fD>�fD?fD?�fD@fD@�fDAfDA�fDBfDB�fDCfDC�fDDfDD�fDEfDE�fDFfDF�fDGfDG�fDHfDH�fDIfDI�fDJfDJ�fDKfDK�fDLfDL�fDMfDM�fDNfDN�fDOfDO�fDPfDP�fDQfDQ�fDR  DR�fDSfDS�fDTfDT�fDUfDU�fDVfDV�fDWfDW�fDXfDX�fDYfDY� DZ  DZ�fD[fD[�fD\fD\�fD]fD]�fD^fD^�fD_fD_�fD`fD`�fDafDa�fDbfDb�fDcfDc�fDdfDd�fDefDe�fDffDf�fDgfDg�fDh  Dh�fDifDi��DjfDj�fDkfDk�fDlfDl�fDmfDm�fDnfDn�fDofDo�fDpfDp� DqfDq�fDrfDr�fDsfDz�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�/A�1'A��A��A��/AΣ�AΑhA�z�A�n�A�XA�33A�x�A��A�VAǬA��A���A�hsA��AļjAÓuA�hsA��`A�v�A�dZA��#A��+A��A��A�$�A��\A�XA�hsA��A��A�5?A�5?A��A���A��FA�A��7A�9XA��-A��A���A��A�?}A�A�\)A���A�ffA���A��A�~�A�O�A�l�A�VA���A���A��A���A�%A��/A�ȴA��A��hA�E�A���A���A��
A���A�l�A�A��DA���A��-A�ZA��A��#A��DA�C�A��A��A��A��wA��7A���A��A�33A��FA�t�A��A��7A�;dA��\A���A�JA�-A��A��A���A���A���A��A�A~(�A}\)A|��A|M�A{�Ay�;Ax��Avr�At�DAr�Ap�AkXAi�;Ai�Ah9XAfn�AeK�Ac�wAap�A`r�A_t�A]�;A\~�A[\)AYƨAXA�AW7LAU�AR�`AQG�AP��AO�7AMK�AL�!ALM�AK�#AK��AKAI;dAD�AD=qAB��ABJAA��AAC�A@�jA@M�A@1A?�A?��A?O�A>��A>r�A<�A:Q�A9�wA9l�A933A9+A8�A6�\A4�A2��A1VA0A/�A/t�A/`BA/\)A/O�A/O�A.�`A-?}A,��A*��A)��A)7LA(Q�A%��A#;dA"�!A"(�A"JA!�A!��A!+A!A ��A bAXA��A�A��A��AdZA+A�AȴAffA{A%A�A��A��A�#AȴA�PA��AffA��A
��A	�TA	O�A	A��AQ�A��At�A33A�+A�FA+A^5A�A�
A�FA�hA
=A��AJA�PA+A�A �yA ^5@��
@�+@��@�?}@�-@�%@�@�@�h@�j@띲@���@�F@�1@�Ĝ@�p�@ܴ9@��@�\)@ڇ+@�^5@�x�@ؼj@��@֧�@��@ղ-@���@�J@��@��@��m@�@�b@���@���@д9@Ѓ@�A�@ϥ�@�
=@���@��H@�~�@�ff@��@��@�@�hs@�r�@�E�@�@�@��T@őh@���@ēu@�|�@���@���@���@���@�A�@��@�=q@�5?@��@�  @��@��T@��@��@��u@�ƨ@��@��@��\@���@�V@���@��9@��u@��@�I�@�dZ@��H@��+@�=q@�@���@��7@��@���@� �@��@�|�@�C�@�ff@�p�@��D@��@��F@��T@�O�@��j@�1'@��w@�t�@�33@���@�ff@�M�@�5?@�J@��^@��@�X@�7L@�/@�&�@�&�@�&�@��@�%@��9@���@�z�@��@�
=@��!@���@��F@���@�7L@�O�@�@�ff@���@�=q@�J@�@���@�@���@��\@��^@���@�l�@���@��!@��H@��@�"�@�l�@�t�@�t�@�;d@�~�@�-@���@��7@�O�@�G�@�&�@�%@�r�@���@���@�;d@��H@���@��R@���@���@��+@�~�@�^5@���@��@��`@��j@�bN@�I�@� �@��
@��F@�t�@�C�@��@��H@��!@��!@�v�@�J@��@��#@���@�hs@�?}@�V@���@���@��@���@��@�r�@�r�@�A�@���@�K�@�33@���@��!@�E�@���@�O�@��/@�r�@�b@�  @�  @��
@��w@���@�dZ@�\)@�K�@��@�n�@�=q@��7@���@��@�r�@� �@��w@���@�K�@�@��@��@��y@��y@��H@�~�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�/A�1'A��A��A��/AΣ�AΑhA�z�A�n�A�XA�33A�x�A��A�VAǬA��A���A�hsA��AļjAÓuA�hsA��`A�v�A�dZA��#A��+A��A��A�$�A��\A�XA�hsA��A��A�5?A�5?A��A���A��FA�A��7A�9XA��-A��A���A��A�?}A�A�\)A���A�ffA���A��A�~�A�O�A�l�A�VA���A���A��A���A�%A��/A�ȴA��A��hA�E�A���A���A��
A���A�l�A�A��DA���A��-A�ZA��A��#A��DA�C�A��A��A��A��wA��7A���A��A�33A��FA�t�A��A��7A�;dA��\A���A�JA�-A��A��A���A���A���A��A�A~(�A}\)A|��A|M�A{�Ay�;Ax��Avr�At�DAr�Ap�AkXAi�;Ai�Ah9XAfn�AeK�Ac�wAap�A`r�A_t�A]�;A\~�A[\)AYƨAXA�AW7LAU�AR�`AQG�AP��AO�7AMK�AL�!ALM�AK�#AK��AKAI;dAD�AD=qAB��ABJAA��AAC�A@�jA@M�A@1A?�A?��A?O�A>��A>r�A<�A:Q�A9�wA9l�A933A9+A8�A6�\A4�A2��A1VA0A/�A/t�A/`BA/\)A/O�A/O�A.�`A-?}A,��A*��A)��A)7LA(Q�A%��A#;dA"�!A"(�A"JA!�A!��A!+A!A ��A bAXA��A�A��A��AdZA+A�AȴAffA{A%A�A��A��A�#AȴA�PA��AffA��A
��A	�TA	O�A	A��AQ�A��At�A33A�+A�FA+A^5A�A�
A�FA�hA
=A��AJA�PA+A�A �yA ^5@��
@�+@��@�?}@�-@�%@�@�@�h@�j@띲@���@�F@�1@�Ĝ@�p�@ܴ9@��@�\)@ڇ+@�^5@�x�@ؼj@��@֧�@��@ղ-@���@�J@��@��@��m@�@�b@���@���@д9@Ѓ@�A�@ϥ�@�
=@���@��H@�~�@�ff@��@��@�@�hs@�r�@�E�@�@�@��T@őh@���@ēu@�|�@���@���@���@���@�A�@��@�=q@�5?@��@�  @��@��T@��@��@��u@�ƨ@��@��@��\@���@�V@���@��9@��u@��@�I�@�dZ@��H@��+@�=q@�@���@��7@��@���@� �@��@�|�@�C�@�ff@�p�@��D@��@��F@��T@�O�@��j@�1'@��w@�t�@�33@���@�ff@�M�@�5?@�J@��^@��@�X@�7L@�/@�&�@�&�@�&�@��@�%@��9@���@�z�@��@�
=@��!@���@��F@���@�7L@�O�@�@�ff@���@�=q@�J@�@���@�@���@��\@��^@���@�l�@���@��!@��H@��@�"�@�l�@�t�@�t�@�;d@�~�@�-@���@��7@�O�@�G�@�&�@�%@�r�@���@���@�;d@��H@���@��R@���@���@��+@�~�@�^5@���@��@��`@��j@�bN@�I�@� �@��
@��F@�t�@�C�@��@��H@��!@��!@�v�@�J@��@��#@���@�hs@�?}@�V@���@���@��@���@��@�r�@�r�@�A�@���@�K�@�33@���@��!@�E�@���@�O�@��/@�r�@�b@�  @�  @��
@��w@���@�dZ@�\)@�K�@��@�n�@�=q@��7@���@��@�r�@� �@��w@���@�K�@�@��@��@��y@��y@��H@�~�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBbNBaHBaHBaHBaHB`BBaHBaHB`BB^5BZBK�B33B0!B:^BH�BJ�BM�BP�BS�BgmB�B��BɺB�?B�?B�3B�9B�wBŢB��B�B�/B�NB�sB�TB��B�wB�?B�3B�jB��B�jB�XB��B��B�#B��B��B�B�B�B�B�B�mB�TBǮB�3B��B��B�Bo�BP�BN�BL�BH�BF�B>wB/B�BJB	7BB��B�
B�qB�!B�B��B��B��B��B��B��B��B�oB�VB� B_;B<jB(�B$�B�B
��B
�fB
��B
��B
��B
��B
t�B
^5B
]/B
[#B
XB
VB
N�B
D�B
?}B
;dB
7LB
.B
,B
1'B
/B
%�B
�B
1B	�B	�B	�mB	�NB	�B	��B	��B	��B	�dB	�?B	�B	��B	��B	��B	�bB	�7B	� B	v�B	o�B	k�B	dZB	YB	XB	XB	W
B	VB	R�B	A�B	49B	49B	.B	-B	+B	'�B	$�B	"�B	!�B	 �B	�B	�B	�B	�B	\B	+B	B	B	B	1B	%B��B�B�;B�#B�)B�/B�/B�)B�)B�)B�#B�B��B��BȴBŢB��B�dB�-B��B��B��B��B��B��B��B��B��B��B�{B�bB�bB�bB�bB�bB�bB�\B�VB�VB�DB�%B� B� Bw�Br�Bn�Bn�Bm�Bl�Bk�BhsBgmBffBffBffBe`BdZBdZBcTBaHB`BB_;B_;B_;B_;B^5B]/B]/B\)B[#BZBZBYBXBW
BW
BT�BR�BR�BS�BS�BQ�BN�BT�BT�BXBT�BVBVBVBZB^5B`BBcTBgmBffBhsBjBp�Bq�Bp�Br�Bv�B}�B�B�B�B�B}�B� B�DB�bB�hB�hB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B��B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�-B�3B�9B�?B�FB�LB�RB�^B�jB�qB�wB��BĜBɺB��B��B��B�BB�ZB�yB�B�B�B��B��B��B��B��B	B	B	B	+B	+B	+B	1B	1B	1B	
=B	PB	oB	uB	uB	�B	$�B	,B	49B	@�B	F�B	K�B	M�B	T�B	ZB	bNB	ffB	gmB	hsB	iyB	jB	hsB	dZB	bNB	_;B	]/B	\)B	^5B	aHB	bNB	cTB	dZB	e`B	ffB	e`B	ffB	gmB	l�B	m�B	n�B	n�B	o�B	p�B	s�B	x�B	y�B	|�B	}�B	~�B	~�B	~�B	~�B	� B	~�B	� B	�B	�+B	�1B	�7B	�PB	�VB	�VB	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�?B	�LB	�XB	�dB	�qB	�wB	�wB	�}B	�}B	��B	��B	��B	��B	ÖB	ƨB	ƨB	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BbNBaHBbNBaHBbNB`BBaHBaHB`BB_;B]/BW
B<jB8RBB�BI�BL�BN�BQ�BYBl�B�%B�B��B�RB�LB�FB�FB��BȴB��B�)B�5B�ZB�yB�mB��B�}B�FB�FB�wB��B�wB�jB��B��B�NB��B��B��B��B�B�B��B�B�B��B�XB�B��B�1B� BQ�BN�BM�BH�BG�BA�B2-B#�BPB
=B%BB�;BÖB�-B�B��B��B��B��B��B��B��B�uB�oB�DBk�BC�B+B)�B!�BB
�B
�B
ȴB
�B
��B
{�B
^5B
^5B
\)B
YB
YB
R�B
F�B
A�B
<jB
:^B
1'B
/B
8RB
49B
+B
"�B
�B	��B	�B	�B	�mB	�5B	�B	��B	ÖB	�wB	�^B	�'B	��B	��B	��B	�{B	�bB	�+B	{�B	q�B	o�B	k�B	[#B	YB	YB	W
B	VB	YB	J�B	6FB	7LB	0!B	.B	,B	(�B	%�B	#�B	!�B	!�B	 �B	�B	�B	�B	�B		7B	%B	B	B	DB	DB	B�B�ZB�5B�/B�5B�/B�)B�)B�)B�/B�5B��B��B��BƨBĜBÖB�XB�B��B��B��B��B��B��B��B��B��B��B�uB�bB�bB�hB�hB�hB�\B�\B�\B�VB�DB�B�B|�Bz�Bq�Bp�Bn�Bn�Bo�BjBiyBgmBgmBgmBgmBe`Be`Be`BdZBbNBbNB`BB_;B`BB_;B_;B_;B^5B]/B[#BZBZBZBYBXBYBYBXBVBT�BT�BXBW
BW
BZB[#B]/B\)B\)B[#B_;BaHBe`BgmBhsBiyBl�Br�Br�Bq�Br�Bv�B� B�B�%B�7B�B}�B~�B�DB�bB�hB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�!B�!B�B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�'B�3B�3B�9B�?B�LB�RB�XB�dB�jB�wB�}BBƨB��B��B��B��B�BB�`B�B�B�B��B��B��B��B��B��B	B	B	B	+B	+B	+B	1B	1B	1B	
=B	VB	oB	uB	{B	�B	%�B	,B	2-B	?}B	E�B	K�B	L�B	S�B	ZB	cTB	ffB	gmB	hsB	iyB	l�B	l�B	ffB	dZB	aHB	^5B	\)B	^5B	aHB	bNB	cTB	dZB	e`B	ffB	ffB	ffB	gmB	l�B	m�B	n�B	n�B	o�B	q�B	t�B	x�B	z�B	}�B	}�B	~�B	~�B	~�B	~�B	� B	~�B	�B	�B	�+B	�1B	�=B	�PB	�VB	�VB	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�-B	�FB	�RB	�^B	�jB	�qB	�wB	�wB	�}B	�}B	��B	��B	��B	B	ĜB	ǮB	ǮB	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�B	�B	�B	�B	�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
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
<�o<#�
<#�
<#�
<#�
<#�
<�o<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.1 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447262012010314472620120103144726  AO  ARGQ                                                                        20111130142540  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130142540  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144726  IP                  G�O�G�O�G�O�                