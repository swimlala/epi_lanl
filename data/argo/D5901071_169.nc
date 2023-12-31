CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:38Z UW 3.1 conversion   
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
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130143004  20190522121827  1727_5046_169                   2C  D   APEX                            2143                            040306                          846 @��s� 1   @��W?�@6_;dZ��c�XbM�1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @y��@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B��B  B  B   B(  B0  B8  B@  BHffBP  BX  B`  BhffBp  Bx  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl�Cn�Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C��C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
�fD  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3fD3� D3��D4y�D5  D5� D6  D6� D7  D7� D8  D8� D8��D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?y�D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]fD]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Dsl�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�  @�33A��A!��AA��Aa��A���A���A���A���A���A���A���A���B ffB  BffBffB ffB(ffB0ffB8ffB@ffBH��BPffBXffB`ffBh��BpffBxffB�33B�  B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�ffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl33Cn33Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C�  C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D fD �fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD	fD	�fD
fD
��DfD�fDfD�fD  D�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD fD �fD!fD!�fD"fD"�fD#fD#�fD$fD$�fD%fD%�fD&fD&�fD'fD'�fD(fD(�fD)fD)�fD*fD*�fD+fD+�fD,fD,�fD-fD-�fD.fD.�fD/fD/�fD0fD0�fD1fD1�fD2fD2�fD3�D3�fD4  D4� D5fD5�fD6fD6�fD7fD7�fD8fD8�fD9  D9�fD:fD:�fD;fD;�fD<fD<�fD=fD=�fD>fD>�fD?fD?� D@fD@�fDAfDA�fDBfDB�fDCfDC�fDDfDD�fDEfDE�fDFfDF�fDGfDG�fDHfDH�fDIfDI�fDJfDJ�fDKfDK�fDLfDL�fDMfDM�fDNfDN�fDOfDO�fDPfDP�fDQfDQ�fDRfDR�fDSfDS�fDTfDT�fDUfDU�fDVfDV�fDWfDW�fDXfDX�fDYfDY�fDZfDZ�fD[fD[�fD\fD\�fD]�D]�fD^fD^�fD_fD_�fD`fD`�fDafDa�fDbfDb�fDcfDc�fDdfDd�fDefDe�fDffDf�fDgfDg�fDhfDh�fDifDi�fDjfDj�fDkfDk�fDlfDl�fDmfDm�fDnfDn�fDofDo�fDpfDp�fDqfDq�fDrfDr�fDsfDss3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�%A�A�A�A�A�A��`A�ȴAҩ�AҁA�`BA�XA�Q�A�G�A�?}A�C�A�A�A�?}A�=qA�33A�/A�-A�(�A�$�A�"�A� �A��A��A�oA�1A���A�5?A�G�A��A�/A�ƨAŝ�Aħ�A�;dA�^5A�
=A��/A��\A�z�A�A���A��A���A�z�A��A���A���A��\A�M�A��A�^5A�M�A�(�A��uA�(�A��+A��;A�S�A�  A���A���A��A�\)A���A�VA�{A��A��A�dZA�A��A�Q�A�"�A���A� �A�t�A��7A���A�jA���A� �A�r�A��uA�VA�A�A�ĜA�1'A��FA�7LA��A�oA���A�ffA�O�A�-A��9A�~�A�VA���A��TA���A���A�t�A��^A�  A��9A��A���A��A��#A���A���A���A���A���A�l�A���A���A�+A�VA��TA�?}A�M�Al�A}x�A{VAxv�At�As��Aq/An �Akt�Aj�Ag��Af5?Ad~�Ab�jA`A�A]|�AZ��AY�wAW�ARM�AR$�AP�DAMXAK33AJ�HAH�AG�AFjAEG�AD��AC��AA�
AA"�A@��A?�A=|�A<�A;�;A:�yA8VA6$�A5�7A4z�A3�A3;dA2r�A1�A1"�A01'A/�;A.-A,�DA+7LA*ZA)�mA)ƨA)�A(�A&��A&I�A%
=A$5?A#�#A#&�A!��A �RA��A�A�
A��A{A��A\)A^5A�;AC�A�RA�A��A�A��A�A�\Az�AQ�A�A�7AȴA1A|�Az�A�-AK�A��A
��A	�A��AA�A�;At�AffAn�A�/A33A ��A ��@���@��F@��7@�"�@���@���@�@�G�@�%@�\)@�-@�Q�@�33@��@��@�Q�@��@�ƨ@畁@�t�@�+@���@�j@�hs@�9@���@�t�@�ff@�$�@�@��@��#@ݺ^@�p�@�V@��@�bN@ۮ@�-@�x�@��@��m@�33@�M�@�hs@�7L@ԓu@Ұ!@�@�X@��@϶F@�"�@ͩ�@�M�@���@�p�@̛�@�bN@�z�@�Z@�Q�@���@�n�@���@ɉ7@�`B@�?}@�Ĝ@���@�Z@�M�@�/@�x�@�7L@�7L@�%@��D@�(�@�\)@��@�V@�$�@��@���@���@�&�@� �@���@��H@�^5@�5?@�E�@�J@��^@�`B@��@�&�@��@���@���@�+@�O�@���@�p�@��@��m@�1'@�I�@�r�@�r�@�bN@�Q�@��/@�@�O�@���@�n�@���@���@��-@��@��@�r�@�A�@�9X@�(�@�b@�1@�b@�b@�1@�  @��;@��P@�C�@�ȴ@�ff@�{@�@�`B@�/@�%@���@�ȴ@���@��@��@�n�@��+@���@�~�@�n�@�E�@�$�@�{@�5?@���@��h@��`@��@��@�v�@���@�@���@���@�/@�V@��@��j@�|�@�ȴ@�M�@��^@��^@��-@���@���@�^5@���@���@���@�v�@�V@�v�@�E�@��-@��@�?}@�G�@��T@�J@�@���@��h@�&�@���@��j@�Z@�bN@�Z@�bN@�j@�1'@��P@�l�@�dZ@�;d@���@���@�~�@���@���@�x�@�O�@��@���@�1@��@�~�@�`B@���@���@��@�  @�  @��m@�l�@��R@�X@�V@��@���@���@�r�@��;@��@��y@�$�@��@��#@�@���@�?}@���@�Z@�I�@�9X@�(�@� �@���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�%A�A�A�A�A�A��`A�ȴAҩ�AҁA�`BA�XA�Q�A�G�A�?}A�C�A�A�A�?}A�=qA�33A�/A�-A�(�A�$�A�"�A� �A��A��A�oA�1A���A�5?A�G�A��A�/A�ƨAŝ�Aħ�A�;dA�^5A�
=A��/A��\A�z�A�A���A��A���A�z�A��A���A���A��\A�M�A��A�^5A�M�A�(�A��uA�(�A��+A��;A�S�A�  A���A���A��A�\)A���A�VA�{A��A��A�dZA�A��A�Q�A�"�A���A� �A�t�A��7A���A�jA���A� �A�r�A��uA�VA�A�A�ĜA�1'A��FA�7LA��A�oA���A�ffA�O�A�-A��9A�~�A�VA���A��TA���A���A�t�A��^A�  A��9A��A���A��A��#A���A���A���A���A���A�l�A���A���A�+A�VA��TA�?}A�M�Al�A}x�A{VAxv�At�As��Aq/An �Akt�Aj�Ag��Af5?Ad~�Ab�jA`A�A]|�AZ��AY�wAW�ARM�AR$�AP�DAMXAK33AJ�HAH�AG�AFjAEG�AD��AC��AA�
AA"�A@��A?�A=|�A<�A;�;A:�yA8VA6$�A5�7A4z�A3�A3;dA2r�A1�A1"�A01'A/�;A.-A,�DA+7LA*ZA)�mA)ƨA)�A(�A&��A&I�A%
=A$5?A#�#A#&�A!��A �RA��A�A�
A��A{A��A\)A^5A�;AC�A�RA�A��A�A��A�A�\Az�AQ�A�A�7AȴA1A|�Az�A�-AK�A��A
��A	�A��AA�A�;At�AffAn�A�/A33A ��A ��@���@��F@��7@�"�@���@���@�@�G�@�%@�\)@�-@�Q�@�33@��@��@�Q�@��@�ƨ@畁@�t�@�+@���@�j@�hs@�9@���@�t�@�ff@�$�@�@��@��#@ݺ^@�p�@�V@��@�bN@ۮ@�-@�x�@��@��m@�33@�M�@�hs@�7L@ԓu@Ұ!@�@�X@��@϶F@�"�@ͩ�@�M�@���@�p�@̛�@�bN@�z�@�Z@�Q�@���@�n�@���@ɉ7@�`B@�?}@�Ĝ@���@�Z@�M�@�/@�x�@�7L@�7L@�%@��D@�(�@�\)@��@�V@�$�@��@���@���@�&�@� �@���@��H@�^5@�5?@�E�@�J@��^@�`B@��@�&�@��@���@���@�+@�O�@���@�p�@��@��m@�1'@�I�@�r�@�r�@�bN@�Q�@��/@�@�O�@���@�n�@���@���@��-@��@��@�r�@�A�@�9X@�(�@�b@�1@�b@�b@�1@�  @��;@��P@�C�@�ȴ@�ff@�{@�@�`B@�/@�%@���@�ȴ@���@��@��@�n�@��+@���@�~�@�n�@�E�@�$�@�{@�5?@���@��h@��`@��@��@�v�@���@�@���@���@�/@�V@��@��j@�|�@�ȴ@�M�@��^@��^@��-@���@���@�^5@���@���@���@�v�@�V@�v�@�E�@��-@��@�?}@�G�@��T@�J@�@���@��h@�&�@���@��j@�Z@�bN@�Z@�bN@�j@�1'@��P@�l�@�dZ@�;d@���@���@�~�@���@���@�x�@�O�@��@���@�1@��@�~�@�`B@���@���@��@�  @�  @��m@�l�@��R@�X@�V@��@���@���@�r�@��;@��@��y@�$�@��@��#@�@���@�?}@���@�Z@�I�@�9X@�(�@� �@���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�^B�^B�XB�XB�XB�XB�RB�LB�FB�?B�9B�9B�9B�3B�-B�9B�9B�9B�9B�-B�'B�'B�'B�!B�!B�!B�!B�B�B�B��B��Bx�Bt�BD�B;dB8RB8RB@�BG�BG�BI�BI�BS�BYB[#B_;B_;B_;BcTBffBq�Bu�Bx�B�B�B�B�+B�PB�PB�{B��B��B��B��B�B�'B�-B�?B�XB�XB�^B�dB�}B�}BB��B��B��B�)B�)B�#B�B��B��BȴB��B�XB�LB�B��Bo�BaHB_;BYBN�BG�BB�B49B$�B�B�B%B�B�NB�B��B��B��B�RB�-B��B��B�uBt�BcTB^5BL�B-B�BB
�yB
�B
�}B
��B
x�B
`BB
O�B
8RB
'�B
oB	��B	�ZB	�B	ɺB	�?B	��B	��B	�+B	w�B	k�B	\)B	I�B	8RB	)�B	"�B	oB	B	JB	JB	{B	B	B��B��B��B��B��B��B��B��B�B�B�B�fB�HB�B��BɺBƨBȴBƨBĜBÖB��B�jB�^B�FB�'B�B��B��B��B��B��B��B��B��B��B��B�{B�hB�\B�PB�DB�=B�7B�+B�+B�%B�B�B�B~�B|�Bw�By�Bz�Bz�Bz�Bz�By�By�Bx�Bw�Bt�Bq�Bo�Bo�Bp�Bo�Bs�Bo�Bo�Bo�Bo�Bn�Bl�BgmB`BB]/B]/B^5B\)B\)BZB]/B]/B\)BZBYBZBZBZBXB[#B]/B_;BaHBaHBaHBaHBaHBaHB`BB^5B]/B\)B^5B`BB`BB^5B]/B_;BjBq�Br�Bt�Bu�Bu�Bw�Bx�B}�B�B�B�B�B�B�B�B}�B}�B~�B}�B|�B� B�B�%B�PB�hB��B��B��B��B��B��B��B�B�B�B�B�B�B�!B�3B�9B�LB�XB�jB�wBŢB��B��B��B�B�/B�5B�;B�BB�BB�NB�mB�yB�B�B��B��B	  B	B	
=B	JB	\B	oB	hB		7B	1B	B	
=B	uB	hB	{B	�B	�B	�B	�B	�B	�B	#�B	/B	.B	'�B	&�B	'�B	'�B	%�B	#�B	#�B	$�B	+B	.B	/B	33B	5?B	6FB	6FB	6FB	9XB	>wB	A�B	D�B	H�B	J�B	K�B	S�B	T�B	XB	XB	XB	W
B	ZB	\)B	hsB	m�B	o�B	o�B	p�B	p�B	p�B	p�B	q�B	r�B	q�B	o�B	m�B	m�B	m�B	l�B	l�B	q�B	w�B	z�B	{�B	}�B	~�B	~�B	}�B	|�B	}�B	� B	� B	�B	�B	�7B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�-B	�9B	�?B	�?B	�LB	�RB	�LB	�LB	�XB	�^B	�dB	�dB	�dB	�jB	�qB	�qB	�qB	�wB	�wB	�}B	��B	��B	��B	B	ÖB	ĜB	ŢB	ĜB	��B	�wB	�wB	�}B	B	ĜB	ƨB	��B	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�^B�^B�XB�XB�XB�XB�XB�RB�LB�FB�9B�9B�9B�3B�-B�9B�9B�9B�9B�-B�'B�'B�'B�!B�!B�!B�!B�B�B�B��B��B~�B�BI�B@�B<jB>wBD�BI�BH�BK�BN�BVB[#B^5B`BB`BBaHBdZBiyBr�Bv�Bz�B�B�B�%B�7B�\B�bB��B��B��B��B�B�!B�-B�9B�RB�^B�^B�dB�qB��B��BĜB��B��B�
B�BB�HB�;B�B��B��B��BŢB�dB�^B�!B��Bu�BcTBbNB\)BP�BH�BH�B:^B&�B�B�BuB�B�mB�B�B�B�
B�^B�?B�B��B��Bz�BdZBcTBS�B1'B�BDB
�B
�)B
��B
��B
�B
ffB
YB
>wB
0!B
�B
+B	�yB	�NB	��B	�qB	��B	��B	�JB	|�B	q�B	cTB	Q�B	?}B	-B	)�B	�B	B	hB	�B	�B	B		7B	B	B	B	  B��B��B��B��B��B��B�B�yB�mB�TB�B��B��B��B��BȴBƨBĜB��B�qB�wB�RB�3B�B�B��B��B��B��B��B��B��B��B��B��B�oB�\B�VB�JB�VB�DB�+B�+B�%B�B�B� B�Bz�B{�B{�Bz�Bz�Bz�Bz�Bz�Bz�By�Bw�Bs�Br�Br�Br�Bs�Bv�Bs�Br�Bq�Bp�Bo�Bo�Bm�Be`BbNB_;B^5B`BB`BB]/B`BBaHB`BBZBZB[#B\)B\)B[#B]/B_;BaHBbNBaHBbNBaHBaHBbNBaHBbNBbNB]/B^5BcTBbNB_;B^5B_;BjBq�Bs�Bu�Bu�Bv�By�B{�B� B�B�B�B�B�%B�B�B}�B� B� B~�B~�B�B�%B�B�VB�oB��B��B��B��B��B��B��B�B�B�B�B�!B�!B�?B�FB�FB�LB�XB�jB�wBƨB��B��B��B�B�/B�5B�;B�BB�HB�TB�sB�B�B�B��B��B	  B	%B	
=B	JB	bB	{B	�B	JB	DB	%B		7B	�B	hB	{B	�B	�B	�B	�B	�B	�B	"�B	0!B	2-B	(�B	&�B	'�B	)�B	&�B	$�B	#�B	$�B	+B	.B	/B	33B	5?B	6FB	6FB	6FB	9XB	?}B	A�B	E�B	I�B	K�B	L�B	T�B	T�B	XB	ZB	ZB	YB	[#B	ZB	hsB	m�B	o�B	o�B	p�B	p�B	p�B	p�B	q�B	r�B	r�B	p�B	n�B	n�B	n�B	m�B	l�B	q�B	w�B	{�B	{�B	}�B	� B	�B	~�B	}�B	~�B	� B	� B	�B	�B	�1B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�-B	�9B	�FB	�FB	�LB	�RB	�RB	�LB	�XB	�^B	�dB	�dB	�jB	�jB	�qB	�qB	�wB	�wB	�}B	��B	��B	��B	��B	B	ÖB	ŢB	ƨB	ŢB	ÖB	�}B	�wB	��B	B	ĜB	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<u<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.1 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447332012010314473320120103144733  AO  ARGQ                                                                        20111130143004  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143004  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144733  IP                  G�O�G�O�G�O�                