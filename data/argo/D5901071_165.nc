CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:37Z UW 3.1 conversion   
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130142919  20190522121827  1727_5046_165                   2C  D   APEX                            2143                            040306                          846 @���[�	1   @���@@6��vȴ9�c�����1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @333@y��@���A   A   A@  A`  A���A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B ffB(  B0  B8  B@  BG��BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B���B�  B�33B�  B���B���B�  B�33B�33B�33B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
�C�C�C�C�C�C�C  C�fC  C  C �C"�C$  C%�fC(  C*  C,�C.�C0  C1�fC4  C6  C8  C:  C<�C>  C@  CB  CD  CE�fCH  CJ  CL  CN�CP  CQ�fCT  CV  CX�CZ  C\  C^  C`  Cb  Cc�fCf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cu�fCx  Cz  C|�C~  C�fC�  C��C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C��3C��3C��3C��3C�  C��C�  C�  C�  C��C��3C��3C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C��C��3C��3C��3C��3C�  C�  C��C�  C�  C��C��C�  C�  C��C�  C��3C�  C�  C��C��C�  C��3C�  C�  C�  C��3C��3C��3D   D � D  D� DfD�fDfD�fDfD�fDfD�fDfD�fD  D� D��D� D	  D	� D	��D
y�D  D�fDfD� D  D� D  D� D��Dy�D��Dy�D��D� DfD� D  D� D��D� DfD� D  D� D  D�fDfD�fD  D� D  D� D��Dy�D��Dy�D��Dy�D��Dy�D��D� D   D � D!  D!� D"  D"� D"��D#y�D#��D$� D%fD%� D&  D&� D'fD'�fD(  D(� D)  D)� D)��D*� D+fD+� D,  D,� D,��D-� D.fD.� D/  D/�fD0  D0� D1  D1�fD2  D2� D3  D3� D3��D4� D5  D5� D6  D6� D7  D7� D8  D8� D8��D9� D:  D:� D;  D;� D;��D<� D=  D=� D=��D>y�D?  D?�fD@fD@�fDAfDA�fDBfDB� DC  DC�fDD  DD� DD��DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK�fDL  DLy�DM  DM�fDN  DN� DO  DO� DP  DP� DQ  DQ�fDRfDR�fDSfDS�fDT  DT� DU  DU� DV  DV�fDW  DWy�DX  DX� DY  DY� DZfDZ�fD[fD[� D[��D\� D]  D]� D^  D^� D_  D_� D`  D`� DafDa� Db  Db� Db��Dc� Dd  Dd� De  De� Df  Df� DgfDg� Dh  Dh� Di  Di� Dj  Dj�fDk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Ds�fDy��D�<�D�s3D���D��fD�  D�\�D���D��fD�&fD�VfD���D��fD��D�` Dڠ D��3D�3D�\�D�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@9��@�  @�  A��A!��AA��Aa��A���A���A���A���A���A���A���A���B ffBffBffBffB ��B(ffB0ffB8ffB@ffBH  BPffBXffB`ffBhffBpffBxffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�  B�  B�33B�33B�33B�  B�33B�ffB�33B�  B�  B�33B�ffB�ffB�ffB�33B�33B�33B�33B�33B�33C �C�C�C�C�C
33C33C33C33C33C33C33C�C  C�C�C 33C"33C$�C&  C(�C*�C,33C.33C0�C2  C4�C6�C8�C:�C<33C>�C@�CB�CD�CF  CH�CJ�CL�CN33CP�CR  CT�CV�CX33CZ�C\�C^�C`�Cb�Cd  Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv  Cx�Cz�C|33C~�C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C�  C��C��C��C��C�  C�  C�  C�  C��C��C��C��C��C��C�  C�  C��C�  C�  C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C�  C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C�  C��C��C��C��C�  C�  C�  C�  C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C�  C��C��C��C�  C�  C�  D fD �fDfD�fD�D��D�D��D�D��D�D��D�D��DfD�fD  D�fD	fD	�fD
  D
� DfD��D�D�fDfD�fDfD�fD  D� D  D� D  D�fD�D�fDfD�fD  D�fD�D�fDfD�fDfD��D�D��DfD�fDfD�fD  D� D  D� D  D� D  D� D  D�fD fD �fD!fD!�fD"fD"�fD#  D#� D$  D$�fD%�D%�fD&fD&�fD'�D'��D(fD(�fD)fD)�fD*  D*�fD+�D+�fD,fD,�fD-  D-�fD.�D.�fD/fD/��D0fD0�fD1fD1��D2fD2�fD3fD3�fD4  D4�fD5fD5�fD6fD6�fD7fD7�fD8fD8�fD9  D9�fD:fD:�fD;fD;�fD<  D<�fD=fD=�fD>  D>� D?fD?��D@�D@��DA�DA��DB�DB�fDCfDC��DDfDD�fDE  DE�fDFfDF�fDGfDG�fDHfDH�fDIfDI�fDJfDJ�fDKfDK��DLfDL� DMfDM��DNfDN�fDOfDO�fDPfDP�fDQfDQ��DR�DR��DS�DS��DTfDT�fDUfDU�fDVfDV��DWfDW� DXfDX�fDYfDY�fDZ�DZ��D[�D[�fD\  D\�fD]fD]�fD^fD^�fD_fD_�fD`fD`�fDa�Da�fDbfDb�fDc  Dc�fDdfDd�fDefDe�fDffDf�fDg�Dg�fDhfDh�fDifDi�fDjfDj��DkfDk�fDlfDl�fDmfDm�fDnfDn�fDofDo�fDpfDp�fDqfDq�fDrfDr�fDsfDs�fDs��Dy�3D�@ D�vfD�� D���D�#3D�` D���D���D�)�D�Y�D�� D��D��D�c3Dڣ3D��fD�fD�` D�f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AϓuAϓuAϓuAϏ\AϑhAϓuAϕ�Aϗ�Aϗ�Aϙ�Aϙ�Aϙ�Aϙ�Aϗ�Aϙ�Aϙ�Aϙ�Aϙ�Aϙ�Aϙ�Aϙ�Aϛ�Aϝ�Aϝ�Aϝ�Aϝ�Aω7A�|�AήA�1'A�ĜA�`BA�ZAhA�A�A��^A��A�bNA���A�l�A�33A��A��TA��jA��!A���A�1'A��RA��DA��A���A�+A��A�hsA��A��A�Q�A�?}A�{A��^A�dZA�33A�  A���A�hsA�%A��/A��A��+A�O�A�VA��yA���A�\)A��yA���A�n�A�oA���A�bNA�A�A�1A���A��DA�ZA� �A���A���A�|�A�\)A�G�A�
=A�K�A��RA��/A�ȴA�(�A��RA�7LA� �A�bA�t�A���A��mA�/A�ȴA�1A��FA��RA���A�1A��DA�^5A�A���A��A���A�
=A��\A��mA��^A�E�A�\)A��HA�"�A���A��A�v�A���A���A�l�A���A���A�oA��9A�A�33A�ffA��A�JA�/A���A���A�-A��hA�1A�1'A�S�A~VA|$�Ax��Av�At^5Aq��Ap�Ao|�An�\Am�Al�Ae��Aa�A^�HA[�-AX�HAV�AR�AP��AO��AJA�AG�AF�AGƨAG7LAE��ACG�AA`BA@~�A?�A=��A<r�A;�#A9�A8��A8ffA7��A6ffA3
=A1?}A/��A-
=A+7LA(��A&�A&n�A%�mA%x�A%�A$��A"�A �`A bAVA\)A~�AA��A�`A�A�A��Ax�A%A�AoA�A�A��A��AbNA��A�Av�AQ�A�A�`A��A��Al�A	��A��A�7A�A�!A��A�A;dAbNA��AXA ��A ~�A ff@���@���@��7@�V@��D@�(�@�\)@�hs@�o@��\@�@�w@�dZ@�;d@��y@�^5@�{@�-@��@�1'@�@띲@���@��;@�`B@��@� �@�"�@���@܋D@۾w@۝�@�;d@��@؋D@�I�@� �@�;d@֧�@�E�@��@�b@�ƨ@ӥ�@�ȴ@Ұ!@ҧ�@��@��@�$�@͑h@͉7@�x�@�O�@�`B@�x�@�`B@�r�@��@�n�@�/@�t�@�^5@���@�5?@�V@�z�@��@�$�@���@�x�@��@��`@��@��@��@��@��@���@�Q�@��
@���@�33@�J@�G�@���@�r�@�K�@��@�ȴ@�v�@�-@��@���@��`@�  @��+@�J@�-@�v�@���@���@��H@�~�@�$�@��#@���@�@�=q@�V@�-@�M�@�~�@���@���@�C�@�dZ@�l�@�K�@��!@�{@��-@�X@��@��@�dZ@�+@���@���@��@��@�O�@�I�@�Z@��@�j@�A�@�dZ@��+@�E�@�$�@��@�{@�{@���@��@�J@�n�@��H@��@��@�=q@�?}@�b@�  @�bN@��u@��9@��9@��9@���@�b@��w@��w@�K�@���@��^@�`B@���@�1@��m@���@�+@�V@��^@���@���@�-@�=q@�J@��@�@�{@�@�?}@�?}@�G�@�hs@�p�@��9@�dZ@�33@�+@��+@���@��@��j@�A�@��;@���@��
@��;@��;@��m@��m@��
@��w@���@�"�@���@�hs@�G�@�%@���@��u@�Q�@� �@��w@�|�@�33@��@��y@�~�@�{@��@�{@��T@���@�O�@��/@��@�bN@��m@��F@�o@���@�V@��h@�7L@�V@�%@�&�@�&�@��@��`@���@���@K�@w\)@qG�@kt�@b^5@Y��@VV@O|�@H��@B^5@;�
@4��@0bN@*��@$�j@!�@hs@�@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AϓuAϓuAϓuAϏ\AϑhAϓuAϕ�Aϗ�Aϗ�Aϙ�Aϙ�Aϙ�Aϙ�Aϗ�Aϙ�Aϙ�Aϙ�Aϙ�Aϙ�Aϙ�Aϙ�Aϛ�Aϝ�Aϝ�Aϝ�Aϝ�Aω7A�|�AήA�1'A�ĜA�`BA�ZAhA�A�A��^A��A�bNA���A�l�A�33A��A��TA��jA��!A���A�1'A��RA��DA��A���A�+A��A�hsA��A��A�Q�A�?}A�{A��^A�dZA�33A�  A���A�hsA�%A��/A��A��+A�O�A�VA��yA���A�\)A��yA���A�n�A�oA���A�bNA�A�A�1A���A��DA�ZA� �A���A���A�|�A�\)A�G�A�
=A�K�A��RA��/A�ȴA�(�A��RA�7LA� �A�bA�t�A���A��mA�/A�ȴA�1A��FA��RA���A�1A��DA�^5A�A���A��A���A�
=A��\A��mA��^A�E�A�\)A��HA�"�A���A��A�v�A���A���A�l�A���A���A�oA��9A�A�33A�ffA��A�JA�/A���A���A�-A��hA�1A�1'A�S�A~VA|$�Ax��Av�At^5Aq��Ap�Ao|�An�\Am�Al�Ae��Aa�A^�HA[�-AX�HAV�AR�AP��AO��AJA�AG�AF�AGƨAG7LAE��ACG�AA`BA@~�A?�A=��A<r�A;�#A9�A8��A8ffA7��A6ffA3
=A1?}A/��A-
=A+7LA(��A&�A&n�A%�mA%x�A%�A$��A"�A �`A bAVA\)A~�AA��A�`A�A�A��Ax�A%A�AoA�A�A��A��AbNA��A�Av�AQ�A�A�`A��A��Al�A	��A��A�7A�A�!A��A�A;dAbNA��AXA ��A ~�A ff@���@���@��7@�V@��D@�(�@�\)@�hs@�o@��\@�@�w@�dZ@�;d@��y@�^5@�{@�-@��@�1'@�@띲@���@��;@�`B@��@� �@�"�@���@܋D@۾w@۝�@�;d@��@؋D@�I�@� �@�;d@֧�@�E�@��@�b@�ƨ@ӥ�@�ȴ@Ұ!@ҧ�@��@��@�$�@͑h@͉7@�x�@�O�@�`B@�x�@�`B@�r�@��@�n�@�/@�t�@�^5@���@�5?@�V@�z�@��@�$�@���@�x�@��@��`@��@��@��@��@��@���@�Q�@��
@���@�33@�J@�G�@���@�r�@�K�@��@�ȴ@�v�@�-@��@���@��`@�  @��+@�J@�-@�v�@���@���@��H@�~�@�$�@��#@���@�@�=q@�V@�-@�M�@�~�@���@���@�C�@�dZ@�l�@�K�@��!@�{@��-@�X@��@��@�dZ@�+@���@���@��@��@�O�@�I�@�Z@��@�j@�A�@�dZ@��+@�E�@�$�@��@�{@�{@���@��@�J@�n�@��H@��@��@�=q@�?}@�b@�  @�bN@��u@��9@��9@��9@���@�b@��w@��w@�K�@���@��^@�`B@���@�1@��m@���@�+@�V@��^@���@���@�-@�=q@�J@��@�@�{@�@�?}@�?}@�G�@�hs@�p�@��9@�dZ@�33@�+@��+@���@��@��j@�A�@��;@���@��
@��;@��;@��m@��m@��
@��w@���@�"�@���@�hs@�G�@�%@���@��u@�Q�@� �@��w@�|�@�33@��@��y@�~�@�{@��@�{@��T@���@�O�@��/@��@�bN@��m@��F@�o@���@�V@��h@�7L@�V@�%@�&�@�&�@��@��`@���@���@K�@w\)@qG�@kt�@b^5@Y��@VV@O|�@H��@B^5@;�
@4��@0bN@*��@$�j@!�@hs@�@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB=qB<jB<jB=qB=qB=qB=qB<jB=qB<jB<jB=qB=qB=qB=qB=qB=qB=qB=qB=qB=qB=qB=qB<jB<jB;dB9XB49B)�B%�B&�B49B9XB;dBB�BF�BK�BN�BW
BZB\)B_;BdZBffBffBffBl�Br�Bu�Bx�Bz�B~�B� B�%B�7B�PB�\B�VB�hB��B��B��B��B��B��B��B��B�B�B�B�'B�3B�?B�LB�wB�}B��B��BĜBȴB��BɺBȴBɺB��B��B��B��B��B��B��B��B��B��BǮBĜB�qB�XB�9B�'B�B��B��B�JBx�BffBZBL�BA�B8RB.B&�B#�B�B�BuB1B��B�B�sB�B��BǮB�FB�B��B��B�oB�1Bn�BffBZBI�B0!B�BhBB
�B
�fB
B
��B
�bB
�B
u�B
jB
^5B
O�B
33B
�B

=B	�B	�HB	��B	�wB	�?B	�B	��B	��B	�7B	^5B	E�B	1'B	�B	\B	%B�B�ZB�#B��BÖB�
B��B��B�B�mB�ZB�ZB�TB�5B�B��B��B��BȴBB�dB�9B�B��B��B��B�oB�VB�PB�PB�DB�=B�1B�7B�1B�%B�B�B�B�B�B�B�B�B�B� B~�B� B~�B~�B|�Bz�Bz�By�By�By�By�Bw�Bv�Bw�Bv�Bt�Bq�Bp�Bq�Bs�Bs�Bq�Bp�Bq�Bp�Bp�Bq�Bq�Bp�Bo�Bm�Bl�Bk�Bk�Bk�BjBiyBgmBe`BcTBbNBcTBbNBbNBaHBaHB_;B`BBaHBbNBdZBe`Be`BdZBbNBbNBffBgmBffBdZBgmBjBk�Bl�Bn�Bp�Bp�Bp�Br�Bt�Bw�B}�B�B�B�B�VB�\B�VB�PB�uB�uB�oB�uB��B��B��B��B��B��B�{B�oB�uB��B��B��B�B��B��B�B�B�-B�qB�wB�wB�wB�wB�wB�wB�qB�qB�wB�}B�}B�}BĜBǮBȴBɺB��B�B�B�)B�5B�;B�;B�HB�mB��B��B	  B	B	B	%B	+B	+B	JB	oB	{B	�B	�B	�B	 �B	'�B	.B	0!B	2-B	7LB	9XB	<jB	=qB	<jB	;dB	9XB	7LB	49B	1'B	5?B	7LB	:^B	<jB	?}B	A�B	>wB	>wB	B�B	C�B	E�B	H�B	L�B	T�B	ZB	]/B	_;B	_;B	`BB	cTB	gmB	jB	n�B	r�B	v�B	�B	�B	�B	�B	�+B	�VB	�hB	�oB	�uB	�{B	��B	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�3B	�FB	�LB	�LB	�RB	�LB	�XB	�qB	�jB	�dB	�^B	�XB	�RB	�qB	��B	ĜB	ŢB	ƨB	ǮB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�#B	�5B	�BB	�NB	�TB	�TB	�TB	�NB	�NB	�TB	�ZB	�NB	�HB	�BB	�;B	�;B	�;B	�HB	�NB	�TB	�TB	�NB	�NB	�sB	��B
%B
bB
�B
�B
&�B
+B
1'B
8RB
=qB
C�B
J�B
N�B
T�B
[#B
`BB
dZB
iyB
n�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B=qB<jB<jB=qB=qB=qB=qB<jB=qB<jB<jB=qB=qB=qB=qB=qB=qB=qB=qB=qB=qB=qB=qB<jB<jB<jB;dB?}BL�BH�B33B=qB@�B?}BH�BN�BQ�BT�BYB]/BbNBe`Be`BgmBhsBjBq�Bu�Bz�B}�B� B�B�B�DB�PB�\B�bB�hB��B��B��B��B��B��B��B�B�B�'B�-B�'B�3B�FB�XB�jB��BBĜBŢBǮB��B��B��B��B��B��B��B��B�
B�
B�
B�B�/B�/B�BB��B��BB�wB�FB�9B�?B�B��B��B�1Bq�BiyBXBK�BB�B33B)�B(�B$�B"�B�BoBB��B��B�NB�BB�B��B�XB��B��B��B��Bu�Bq�BiyB]/B?}B'�B�B\BB+B
�B
�9B
��B
�PB
}�B
t�B
l�B
gmB
C�B
.B
�B
B	�B	�/B	ǮB	�jB	�?B	�B	�B	�-B	x�B	]/B	H�B	33B	"�B	 �B��B�B��B�BǮB��B	B	%B	B�B�B�B�B�sB�BB�NB��B��B��B��B��BB�^B�LB�B��B��B�oB�hB�hB�\B�hB��B�{B�\B�VB�\B�=B�1B�1B�7B�7B�=B�+B�B�B�%B�+B�B�B� B�B~�B~�B~�B}�B|�B|�B~�B�B� B� B}�Bw�Bx�Bw�Bw�By�B|�Bw�Bv�Bt�Bt�Bt�Bs�Bq�Bq�Bp�Bn�Bm�Bm�Bl�Bn�Bn�Bl�BffBhsBe`BcTBcTBbNBcTB`BBaHBaHBbNBdZBe`Be`BdZBbNBbNBffBgmBiyBjBgmBk�Bm�Bp�Bn�Bp�Bq�Bp�Br�Bv�B{�B�B�B�B�B�VB�\B�VB�PB��B�uB�oB�uB��B��B��B��B��B��B��B��B�uB��B��B�-B�B�B�B�B�B�-B�qB�wB�wB�wB�wB�wB�wB�wB�}B��B��B��BÖBĜBǮBȴB��B��B�B�#B�/B�5B�;B�NB�ZB�B��B��B��B	B	B	+B	1B	+B	PB	oB	uB	�B	�B	�B	 �B	&�B	.B	/B	1'B	7LB	9XB	<jB	?}B	>wB	<jB	:^B	:^B	8RB	1'B	6FB	9XB	:^B	=qB	C�B	I�B	A�B	>wB	B�B	D�B	F�B	K�B	L�B	T�B	ZB	]/B	_;B	_;B	`BB	cTB	gmB	iyB	m�B	r�B	w�B	�B	�B	�+B	�B	�%B	�PB	�hB	�oB	�uB	��B	��B	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�9B	�FB	�LB	�LB	�XB	�LB	�jB	�wB	�qB	�qB	�^B	�XB	�RB	�wB	��B	ĜB	ŢB	ƨB	ǮB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�#B	�5B	�BB	�NB	�ZB	�ZB	�ZB	�TB	�TB	�ZB	�`B	�TB	�NB	�NB	�BB	�;B	�;B	�HB	�NB	�TB	�TB	�NB	�NB	�mB	��B
%B
bB
�B
�B
&�B
+B
1'B
8RB
=qB
C�B
J�B
N�B
T�B
[#B
`BB
dZB
iyB
n�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X=C�=C�<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<T��<�1<u<49X<u<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<#�
<T��<#�
<e`B<�o<#�
<T��<#�
<#�
<49X<���<#�
<49X<u<���<u<#�
<49X<49X<e`B=o<�1<u<#�
<#�
<#�
<#�
<e`B<�j<�o<�C�<���<�o<�C�<e`B<#�
<#�
<#�
<#�
<��
=#�
<���<�j<�j<��
<���<���<T��<e`B=o<T��<#�
<#�
<#�
<D��<u<D��<#�
<#�
<D��<#�
<#�
<T��<#�
<#�
<#�
<T��<��
<e`B<T��<�C�<u<u<49X<#�
<#�
<#�
<#�
<#�
<T��<49X<#�
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
<49X<D��<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.1 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447322012010314473220120103144732  AO  ARGQ                                                                        20111130142919  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130142919  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144732  IP                  G�O�G�O�G�O�                