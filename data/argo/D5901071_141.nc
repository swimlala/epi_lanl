CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:30Z UW 3.1 conversion   
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
_FillValue                 �  K|   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  Mx   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  Uh   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]X   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  _T   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  gD   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  i@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  q0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  {   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �T   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �X   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �\   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �`   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �d   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130142353  20190522121827  1727_5046_141                   2C  D   APEX                            2143                            040306                          846 @���@��1   @����|��@77���+�c��/��1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���B   B��B  B  B��B(  B0ffB8ffB@  BH  BO��BX  B`ffBh  Bp  Bw��B�  B�  B�  B�  B�  B�33B�  B���B�  B�  B�33B�  B���B���B���B���B�  B�  B���B���B�  B�  B�33B�33B�  B���B���B�  B�33B�33B�33B�33C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C�fC�fC�fC!�fC$  C&  C(  C*  C+�fC.  C0  C2  C3�fC6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR�CT�CU�fCX  CZ  C\  C^  C_�fCb  Cd  Cf  Ch  Cj�Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C{�fC~  C��C�  C��3C�  C��C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C��C�  C�  C��3C�  C��C��C�  C�  C��C��C��C��C��C�  C��3C�  C��C��C��C��C��C��C��C�  C�  C��3C��3C��3C�  C��C��C�  C�  C�  C�  C�  C�  C��3C�  C��C��C�  C�  C�  C��3C��3C�  C�  C�  C�  C��C�  C�  C��3C�  C��C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��3C�  C�  C�  C��C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��C�  C��3C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  Dy�D��Dy�D  D� D  Dy�D  D�fD	fD	�fD
fD
�fDfD� D��D� D  Dy�D  D�fD  D� D  Dy�D��Dy�D  D�fDfD�fDfD�fDfD�fDfD� D��Dy�D��Dy�D��D� DfD�fD  Dy�D  D� D��D� D  D� DfD� D��D � D!  D!y�D"  D"� D#  D#y�D$  D$�fD%fD%�fD&  D&� D'  D'y�D(  D(�fD)  D)� D*  D*� D+  D+y�D,  D,� D-  D-y�D.  D.� D.��D/� D0fD0� D1  D1� D2fD2� D3  D3�fD4  D4� D5fD5� D6  D6� D6��D7y�D7��D8y�D9  D9� D:  D:� D;  D;y�D<  D<�fD=  D=� D>  D>� D?  D?y�D?��D@� DAfDA�fDBfDB�fDC  DCy�DC��DDy�DD��DEy�DF  DF� DG  DG�fDH  DH� DI  DI� DJfDJ�fDKfDK�fDL  DL� DM  DM� DM��DNy�DO  DO� DP  DP� DP��DQ� DR  DR� DR��DS� DT  DT� DU  DU� DV  DV� DW  DW�fDX  DXy�DY  DY� DZ  DZ� D[fD[� D\  D\� D\��D]� D^  D^� D_  D_� D`  D`y�Da  Da� Db  Db� Dc  Dc�fDd  Dd� De  De� Df  Df�fDg  Dgy�Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do�fDp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Ds��Dy�3D�)�D�VfD�� D��fD�0 D�p D��fD���D�  D�` D���D��3D�3D�S3Dڰ D�ٚD�fD�S3D�f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@9��@�33@�33A��A!��AA��Aa��A���A���A���A���A���A���A���A�B ffB  BffBffB   B(ffB0��B8��B@ffBHffBP  BXffB`��BhffBpffBx  B�33B�33B�33B�33B�33B�ffB�33B�  B�33B�33B�ffB�33B�  B�  B�  B�  B�33B�33B�  B�  B�33B�33B�ffB�ffB�33B�  B�  B�33B�ffB�ffB�ffB�ffC �C�C�C�C�C
�C�C�C�C�C�C�C�C�C  C  C   C"  C$�C&�C(�C*�C,  C.�C0�C2�C4  C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR33CT33CV  CX�CZ�C\�C^�C`  Cb�Cd�Cf�Ch�Cj33Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|  C~�C��C��C�  C��C��C��C��C�  C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C�  C�  C�  C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C�  C�  C��C��C��C��C��C��C��C�  C��C��C��C�  C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C��C�  C�  C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C�  C��C��C��C��D fD �fDfD�fDfD�fDfD�fDfD� D  D� DfD�fDfD� DfD��D	�D	��D
�D
��D�D�fD  D�fDfD� DfD��DfD�fDfD� D  D� DfD��D�D��D�D��D�D��D�D�fD  D� D  D� D  D�fD�D��DfD� DfD�fD  D�fDfD�fD�D�fD   D �fD!fD!� D"fD"�fD#fD#� D$fD$��D%�D%��D&fD&�fD'fD'� D(fD(��D)fD)�fD*fD*�fD+fD+� D,fD,�fD-fD-� D.fD.�fD/  D/�fD0�D0�fD1fD1�fD2�D2�fD3fD3��D4fD4�fD5�D5�fD6fD6�fD7  D7� D8  D8� D9fD9�fD:fD:�fD;fD;� D<fD<��D=fD=�fD>fD>�fD?fD?� D@  D@�fDA�DA��DB�DB��DCfDC� DD  DD� DE  DE� DFfDF�fDGfDG��DHfDH�fDIfDI�fDJ�DJ��DK�DK��DLfDL�fDMfDM�fDN  DN� DOfDO�fDPfDP�fDQ  DQ�fDRfDR�fDS  DS�fDTfDT�fDUfDU�fDVfDV�fDWfDW��DXfDX� DYfDY�fDZfDZ�fD[�D[�fD\fD\�fD]  D]�fD^fD^�fD_fD_�fD`fD`� DafDa�fDbfDb�fDcfDc��DdfDd�fDefDe�fDffDf��DgfDg� DhfDh�fDifDi�fDjfDj�fDkfDk�fDlfDl�fDmfDm�fDnfDn�fDofDo��DpfDp�fDqfDq�fDrfDr�fDsfDs�fDs�3Dy��D�,�D�Y�D��3D��D�33D�s3D���D�� D�#3D�c3D�� D��fD�fD�VfDڳ3D���D��D�VfD�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�ĜA�ƨA�ƨA�ĜA�ƨA�A�ƨA��FA���A��+A�r�A�-A��`A��TA��TA��TA��HA��TA��TA��TA��`A��TA��`A��`A��`A��mA��mA��yA��yA��A��A��A��A��A��yA��mA��mA��`A��HA��/A��#A���A�ȴA��-A�t�A�(�A��wA�/A��wA�dZA�`BA�&�A��A�JA��A��`A�bNA��mA���A�Q�A���A�ĜA���A�hsA�hsA���A��TA���A���A�  A��HA�ffA� �A��7A��A���A�K�A�jA���A�hsA��-A�+A��+A�~�A���A�I�A���A�r�A�{A���A�A��^A���A�bNA�\)A��DA���A��/A���A�XA���A���A�1A���A�\)A�\)A���A��FA��PA�x�A�hsA��A�S�A��A��-A��PA�ffA��!A�ZA��!A�r�A���A�;dAVA}x�A|��A{t�Az��Az  AxE�Aw��Av��Au�PAtAr��Ao�Al�AiAg�^Af�`Af-Ac�TA`��A`  A_��A_��A_|�A_%A^(�A\��AZ�yAXI�AVASXAR1AQ��APQ�ANjAMC�AK;dAI�-AH�/AH=qAG�^AGdZAFȴAF�AE��AEx�AEK�AD�RAC��AAƨA@9XA?l�A?A>��A>r�A=��A<JA:jA9XA8M�A6bNA3VA1%A/ƨA.Q�A+��A*�`A*z�A)�FA(�DA(E�A'|�A&��A%��A$$�A#%A"�A!��A!33A �/A ��A r�A $�A��A�A?}A�AM�AK�A�An�A^5AbNA^5AI�A��A�A5?AAl�A%A�+A|�AȴA~�A��A?}A��Az�A�AA��A �A�AI�A�mA
��A
ffA
�A	�^A	C�A��A;dA�RAAVA�A��A�+AS�@��@���@�&�@��;@�X@�j@�1@��P@�hs@���@�P@�-@�V@�ƨ@���@�\)@�h@�Ĝ@�Z@�@�v�@�`B@�1'@�l�@�5?@�b@ߕ�@�$�@ݑh@�O�@ܬ@ۍP@�$�@���@�%@�I�@�b@�;d@ҏ\@��#@���@́@��@�1'@ˍP@�-@ɲ-@��`@� �@�~�@�@�I�@�=q@��#@���@�;d@��T@�%@��`@�bN@� �@��@�l�@�+@��@�$�@��T@�?}@� �@�ȴ@�5?@�&�@��@�o@��y@�M�@��
@�\)@���@�V@��@�p�@�7L@��u@��@�=q@�hs@�Ĝ@��;@�33@��@���@�=q@�?}@�I�@�Z@�j@���@�"�@�J@���@�O�@��u@�1'@�ƨ@�dZ@�o@���@��R@���@�hs@��@���@�Ĝ@���@��D@��@��m@�C�@��@���@���@�x�@�O�@�7L@���@�1'@��;@��w@���@�K�@�33@��@��@�=q@�{@��T@���@�V@�Ĝ@��u@�j@�bN@� �@���@��;@��
@�\)@�o@��y@�ȴ@���@���@��+@�=q@��@���@�`B@�G�@�&�@��`@���@�  @��P@�t�@�l�@�l�@�dZ@�S�@�;d@�+@���@�"�@�S�@�S�@�C�@���@�~�@�{@��-@�X@�7L@��@��@�&�@��@��@���@�Ĝ@���@��D@���@�r�@�K�@��+@��\@�o@��
@� �@�(�@�1'@���@�S�@�~�@�M�@�J@�=q@���@���@�`B@�V@�&�@�/@��@�%@�Ĝ@�bN@�9X@�b@��@�dZ@�K�@�;d@��@�~�@�-@�J@�@��@�X@���@�z�@�z�@�bN@��m@���@v��@o
=@f��@^V@Up�@Pr�@Ix�@A��@;C�@49X@.��@+33@'|�@#S�@|�@33@�;@��@&�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�ĜA�ƨA�ƨA�ĜA�ƨA�A�ƨA��FA���A��+A�r�A�-A��`A��TA��TA��TA��HA��TA��TA��TA��`A��TA��`A��`A��`A��mA��mA��yA��yA��A��A��A��A��A��yA��mA��mA��`A��HA��/A��#A���A�ȴA��-A�t�A�(�A��wA�/A��wA�dZA�`BA�&�A��A�JA��A��`A�bNA��mA���A�Q�A���A�ĜA���A�hsA�hsA���A��TA���A���A�  A��HA�ffA� �A��7A��A���A�K�A�jA���A�hsA��-A�+A��+A�~�A���A�I�A���A�r�A�{A���A�A��^A���A�bNA�\)A��DA���A��/A���A�XA���A���A�1A���A�\)A�\)A���A��FA��PA�x�A�hsA��A�S�A��A��-A��PA�ffA��!A�ZA��!A�r�A���A�;dAVA}x�A|��A{t�Az��Az  AxE�Aw��Av��Au�PAtAr��Ao�Al�AiAg�^Af�`Af-Ac�TA`��A`  A_��A_��A_|�A_%A^(�A\��AZ�yAXI�AVASXAR1AQ��APQ�ANjAMC�AK;dAI�-AH�/AH=qAG�^AGdZAFȴAF�AE��AEx�AEK�AD�RAC��AAƨA@9XA?l�A?A>��A>r�A=��A<JA:jA9XA8M�A6bNA3VA1%A/ƨA.Q�A+��A*�`A*z�A)�FA(�DA(E�A'|�A&��A%��A$$�A#%A"�A!��A!33A �/A ��A r�A $�A��A�A?}A�AM�AK�A�An�A^5AbNA^5AI�A��A�A5?AAl�A%A�+A|�AȴA~�A��A?}A��Az�A�AA��A �A�AI�A�mA
��A
ffA
�A	�^A	C�A��A;dA�RAAVA�A��A�+AS�@��@���@�&�@��;@�X@�j@�1@��P@�hs@���@�P@�-@�V@�ƨ@���@�\)@�h@�Ĝ@�Z@�@�v�@�`B@�1'@�l�@�5?@�b@ߕ�@�$�@ݑh@�O�@ܬ@ۍP@�$�@���@�%@�I�@�b@�;d@ҏ\@��#@���@́@��@�1'@ˍP@�-@ɲ-@��`@� �@�~�@�@�I�@�=q@��#@���@�;d@��T@�%@��`@�bN@� �@��@�l�@�+@��@�$�@��T@�?}@� �@�ȴ@�5?@�&�@��@�o@��y@�M�@��
@�\)@���@�V@��@�p�@�7L@��u@��@�=q@�hs@�Ĝ@��;@�33@��@���@�=q@�?}@�I�@�Z@�j@���@�"�@�J@���@�O�@��u@�1'@�ƨ@�dZ@�o@���@��R@���@�hs@��@���@�Ĝ@���@��D@��@��m@�C�@��@���@���@�x�@�O�@�7L@���@�1'@��;@��w@���@�K�@�33@��@��@�=q@�{@��T@���@�V@�Ĝ@��u@�j@�bN@� �@���@��;@��
@�\)@�o@��y@�ȴ@���@���@��+@�=q@��@���@�`B@�G�@�&�@��`@���@�  @��P@�t�@�l�@�l�@�dZ@�S�@�;d@�+@���@�"�@�S�@�S�@�C�@���@�~�@�{@��-@�X@�7L@��@��@�&�@��@��@���@�Ĝ@���@��D@���@�r�@�K�@��+@��\@�o@��
@� �@�(�@�1'@���@�S�@�~�@�M�@�J@�=q@���@���@�`B@�V@�&�@�/@��@�%@�Ĝ@�bN@�9X@�b@��@�dZ@�K�@�;d@��@�~�@�-@�J@�@��@�X@���@�z�@�z�@�bN@��m@���@v��@o
=@f��@^V@Up�@Pr�@Ix�@A��@;C�@49X@.��@+33@'|�@#S�@|�@33@�;@��@&�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B�B�-B�FB�XB�jB�qB�qB�wB�}B��B��BBBÖBÖBÖBŢBǮBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�BB�mB�B��B�B$�B-B33B5?B5?B7LB;dB<jBA�BH�BF�BE�BG�BW
B[#B[#BXBT�BR�BL�BI�BF�B@�B+BuB+B�BŢB�-B��B��Bx�B^5BR�BK�B@�B5?B�B��B�NB��BÖB�^B�B�uB� Bp�BdZB[#BR�BE�B>wB8RB1'B$�B�B1B
��B
�B
�BB
�B
��B
��B
��B
ǮB
B
�RB
��B
��B
�B
}�B
t�B
o�B
iyB
e`B
_;B
YB
S�B
M�B
G�B
@�B
2-B
!�B
�B	��B	�`B	�#B	��B	�B	�B	��B	�B	��B	��B	��B	��B	��B	ŢB	�qB	�B	��B	�B	|�B	s�B	r�B	iyB	VB	I�B	E�B	K�B	XB	[#B	[#B	ZB	ZB	XB	T�B	S�B	Q�B	M�B	F�B	<jB	49B	1'B	.B	+B	&�B	 �B	�B	uB	DB	B��B�B�sB�NB�)B��B��B��B��BǮBÖB��B�jB�RB�?B�!B�B�B��B��B��B��B��B��B��B��B��B��B�{B�uB�uB�oB�oB�hB�\B�PB�JB�=B�7B�+B�B�B� B}�B{�By�Bw�Bv�Bs�Bo�Bn�BjBhsBgmBe`BcTBcTBbNB`BB^5B\)B[#BXBVBS�BP�BO�BN�BK�BI�BG�BF�BE�BC�BC�BC�BA�B@�B@�B?}B>wB<jB;dB9XB8RB9XB:^B:^B9XB9XB8RB8RB8RB7LB8RB:^B9XB:^B9XB8RB7LB5?B5?B:^B:^B:^B9XB:^B9XB8RB<jB>wB>wB?}BA�BD�BF�BI�BI�BO�BO�BS�BZBZBXBbNBe`BjBjBn�Bp�Bs�Bt�Bx�B{�B�B�B�B�+B�+B�%B�+B�=B�7B�1B�+B�\B�VB�{B��B��B��B��B��B��B��B�B�B�B�'B�-B�-B�9B�RB�qB�wB�}BƨB��B��B��B��B�
B�B�)B�5B�BB�NB�TB�mB�B�B�B�B�B�B�B��B��B��B	B	+B	
=B	
=B	DB	PB	hB	{B	�B	�B	�B	�B	�B	�B	#�B	%�B	'�B	)�B	0!B	33B	6FB	8RB	8RB	;dB	=qB	=qB	>wB	B�B	E�B	F�B	G�B	H�B	H�B	H�B	J�B	J�B	L�B	M�B	N�B	O�B	P�B	R�B	ZB	`BB	bNB	bNB	bNB	cTB	dZB	e`B	ffB	l�B	n�B	s�B	t�B	t�B	s�B	r�B	r�B	r�B	t�B	w�B	z�B	}�B	�B	�B	�%B	�1B	�7B	�7B	�DB	�JB	�PB	�hB	�uB	��B	��B	��B	��B	�B	�B	�!B	�B	�B	�B	�'B	�9B	�FB	�FB	�LB	�RB	�^B	�dB	�jB	�jB	�qB	��B	��B	B	ÖB	ǮB	ȴB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�HB	��B
1B
{B
�B
%�B
,B
33B
:^B
A�B
H�B
M�B
Q�B
VB
[#B
^5B
bNB
e`B
hsB
m�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B��B��B��B��B��B��B��B��B�B�B�-B�FB�XB�jB�qB�qB�wB�}B��B��BBBÖBÖBÖBŢBǮBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�5B�`B�B�B��B�B+B49B9XB<jB;dB<jB>wB?}BE�BK�BI�BI�BQ�B[#B]/B^5B[#B]/B^5BQ�BN�BN�BK�B49B�B{B��B��B�dB�B��B�PBgmBXBR�BI�BI�B=qBPB�B�)B��B��B��B��B�PB{�Bl�BdZB`BBL�BC�BA�B>wB49B2-B�B%B
��B
�yB
�5B
�B
��B
��B
��B
��B
��B
�XB
��B
�JB
�7B
~�B
x�B
o�B
n�B
e`B
`BB
^5B
R�B
M�B
K�B
>wB
/B
,B
{B	��B	�mB	�)B	�NB	�`B	�`B	�B	��B	��B	��B	��B	��B	��B	��B	�wB	��B	�oB	�B	x�B	}�B	v�B	`BB	W
B	O�B	Q�B	\)B	_;B	^5B	_;B	_;B	[#B	W
B	W
B	XB	W
B	T�B	F�B	9XB	49B	1'B	/B	.B	/B	'�B	�B	�B	�B	bB��B�B�B�B�B��B��B��B��B��BɺBŢBB�jB�9B�9B�B�B�B��B��B��B��B��B��B��B��B��B��B�uB�oB�uB�uB�uB�uB�hB�PB�JB�DB�=B�7B�B�B�B}�B{�Bz�B{�B{�Bu�Bp�Bm�Bo�BiyBjBffBe`BdZBcTBdZBaHB]/B]/BZBYBW
BS�BT�BR�BK�BK�BJ�BJ�BF�BE�BD�BF�BC�BC�BC�B<jB;dB9XB@�B?}B=qB:^B<jB9XB8RB8RB8RB7LB8RB=qB9XB<jB;dB8RB<jB5?BA�B=qB<jB:^B9XB:^B=qB8RB<jB>wB>wB?}BA�BD�BF�BM�BI�BO�BO�BS�BZB]/BXBbNBhsBjBl�Bo�Bp�Bt�Bu�By�B{�B�B�B�1B�DB�+B�=B�JB�JB�7B�DB�+B�\B�VB��B��B��B��B��B��B�B�B�B�!B�!B�'B�3B�9B�LB�RB�qB�wB�}BɺB��B��B��B��B�
B�)B�/B�5B�HB�TB�TB�yB�B�B�B�B�B�B�B��B��B	  B	B	+B	DB	
=B	JB	\B	oB	�B	�B	�B	�B	�B	�B	�B	#�B	%�B	(�B	)�B	0!B	33B	6FB	8RB	9XB	<jB	=qB	=qB	?}B	C�B	F�B	F�B	G�B	H�B	H�B	H�B	K�B	K�B	M�B	M�B	N�B	O�B	Q�B	T�B	[#B	`BB	bNB	bNB	bNB	cTB	dZB	e`B	ffB	l�B	n�B	s�B	t�B	u�B	t�B	r�B	s�B	r�B	t�B	w�B	z�B	}�B	�B	�%B	�%B	�1B	�7B	�7B	�DB	�PB	�bB	�uB	�uB	��B	��B	��B	��B	�B	�!B	�!B	�'B	�B	�!B	�'B	�?B	�FB	�LB	�LB	�RB	�^B	�dB	�jB	�qB	�wB	��B	��B	B	ĜB	ǮB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�HB	��B
1B
{B
�B
%�B
,B
33B
:^B
A�B
H�B
M�B
Q�B
VB
[#B
^5B
bNB
e`B
hsB
m�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<49X<#�
<#�
<T��<�t�<u<#�
<#�
<��
<��
<#�
<#�
<#�
<#�
<��
<�h<�t�<u<49X<#�
<�C�<�1<���<T��<49X<#�
<#�
<T��<#�
<#�
<#�
<T��<u<�j<u<e`B<D��<#�
<#�
<#�
<#�
<#�
<#�
<D��<���<���<e`B<#�
<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<D��<T��<�9X<�9X<�o<D��<#�
<#�
<u<�o<#�
<#�
<#�
<#�
<#�
<#�
<T��<e`B<�C�<�C�<�o<#�
<#�
<49X<T��<#�
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
<e`B<#�
<#�
<#�
<#�
<#�
<#�
<e`B<49X<#�
<49X<�C�<�9X<e`B<49X<T��<�o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.1 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447232012010314472320120103144723  AO  ARGQ                                                                        20111130142353  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130142353  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144723  IP                  G�O�G�O�G�O�                