CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:06:16Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A0   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \P   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^@   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  g�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  wp   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y`   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �    SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �,   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �0   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �4   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �8   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �<   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �|   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005190616  20181005190616  5904953 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6432                            2B  A   APEX                            7467                            062512                          846 @מm�1   @מny]B@3��hr�!�cى7Kƨ1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   A   @�33@�  @���A   A@  A`  A~ffA�  A�  A�  A�  A�  A�  A�  B ffB  B  B  B   B(ffB0  B8  B@  BH  BP  BX  B`  Bg��Bp  Bx  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C1�fC4  C6  C8  C:�C<�C>  C@  CB  CD  CF  CH  CI�fCL  CN  CP  CR  CS�fCV  CX�CZ  C[�fC^  C`�Cb�Cd33Cf  Ch  Cj�Ck�fCn  Cp�Cr  Cs��Cu��Cx  Cz  C|33C~  C�  C��C�  C�  C��fC��C�  C��C�  C�  C�  C��3C��C��3C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C��C��3C�  C�  C�  C�  C��3C��C��3C�  C�  C��3C�  C�  C��3C�  C��3C��C��3C�  C�  C��3C��C�  C��C��3C��C��C��fC��C�  C��fC��C��3C��3C��C�  C��fC��3C��3C�  C��C�  C��fC��fC��fC��fC��fC��fC��fC��fC�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��3C�  C��C��C��C��3C�  C��C��3C��3C�  C��3C�  C��C�  C��3C�  C��fC�  C�  C��3C�  C��C��3C��3C�  C�  C��3C�  C��C��3D   D � DfD�fD��Dy�D  D�fD��D� DfD� D  D��D  Ds3D��D�fD	  D	s3D
  D
�fD�D��D  Dy�D  D�fDfD� D�3Dy�D  D�fD�D� D�3D� DfD�fD��Ds3D  D�fDfD� DfD� D��D�fDfDs3D  D��D�D�fD  D� D  D�fDfD� D  D�fD   D � D!fD!�fD"fD"�fD#  D#y�D#�3D$y�D%fD%��D&�D&��D'�D'��D(�D(��D)fD)�fD*fD*� D*��D+y�D+��D,y�D,��D-s3D-��D.y�D.�3D/s3D/�3D0� D1�D1�fD2fD2y�D2��D3s3D4  D4��D5fD5� D6fD6�fD6��D7� D8  D8�fD9fD9� D9��D:� D;  D;� D;�3D<� D=fD=�fD>  D>� D>��D?s3D?�3D@� DAfDA� DA��DB� DC�DC�fDDfDD�fDE  DE� DF  DF�fDGfDG�fDH  DH� DH��DIy�DI��DJy�DJ��DKy�DK��DL� DMfDM� DNfDN�fDN��DOs3DO��DP� DQfDQ��DRfDR�fDSfDS�fDT�DT�fDUfDU�fDV�DV��DW�DW�fDXfDX� DY  DY� DY��DZ�fD[fD[y�D[��D\y�D\��D]�fD^fD^�fD_fD_��D`fD`�fD`��Day�Db  Db�fDc  Dcy�Dd  Ddy�De  De� De��Df�fDg  Dgy�DhfDh�fDi  Di� Dj  Dj� Dk  Dk�fDk��Dl� Dm  Dm� DnfDny�Do  Do�fDo��Dp� DqfDq� Dq�3Dry�Ds  Dsy�Dt  Dt�fDu  Duy�Dv  Dv�fDw  Dwy�Dy�)D�W
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�\)@�(�A z�A"{AB{Ab{A�=pA�
=A�
=A�
=A�
=A�
=A�
=A�
=B �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�B�B�B�B�B�B�B�B�B�B�B�B�\B�\B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�\B�B�B�B�B�B�B�B�B�B�B�B�B�u�B�B�B�B�B�B�C !HC!HC!HC!HC!HC
!HC!HC!HC!HC!HC!HC!HC!HC!HC!HC!HC !HC"!HC$!HC&!HC(!HC*!HC,!HC.!HC0!HC2�C4!HC6!HC8!HC::�C<:�C>!HC@!HCB!HCD!HCF!HCH!HCJ�CL!HCN!HCP!HCR!HCT�CV!HCX:�CZ!HC\�C^!HC`:�Cb:�CdT{Cf!HCh!HCj:�Cl�Cn!HCp:�Cr!HCs�Cu�Cx!HCz!HC|T{C~!HC��C�qC��C��C��
C�qC��C�qC��C��C��C��C�qC��C��C��C��C��C��C��C�qC��C��C��C��C��C��C��C��C��C�qC��C��C��C��C��C��C�qC��C��C��C��C��C��C��C��C��C�qC��C��C��C��C�qC��C�*>C��C�qC�qC��
C�qC��C��
C�qC��C��C�*>C��C��
C��C��C��C�qC��C��
C��
C��
C��
C��
C��
C��
C��
C��C�*>C�qC�qC�qC�qC�*>C�*>C�*>C�*>C�*>C�*>C�qC�*>C�*>C�*>C��C��C�qC�qC�qC��C��C�qC��C��C��C��C��C�qC��C��C��C��
C��C��C��C��C�qC��C��C��C��C��C��C�qC��D RD �RD�D��D�D��DRD��D�D�RD�D�RDRD�DRD{�D�D��D	RD	{�D
RD
��DD�DRD��DRD��D�D�RD��D��DRD��DD�RD��D�RD�D��D�D{�DRD��D�D�RD�D�RD�D��D�D{�DRD�DD��DRD�RDRD��D�D�RDRD��D RD �RD!�D!��D"�D"��D#RD#��D#��D$��D%�D%�D&D&�D'D'�D(D(�D)�D)��D*�D*�RD+�D+��D,�D,��D-�D-{�D.�D.��D.��D/{�D/��D0�RD1D1��D2�D2��D3�D3{�D4RD4�D5�D5�RD6�D6��D7�D7�RD8RD8��D9�D9�RD:�D:�RD;RD;�RD;��D<�RD=�D=��D>RD>�RD?�D?{�D?��D@�RDA�DA�RDB�DB�RDCDC��DD�DD��DERDE�RDFRDF��DG�DG��DHRDH�RDI�DI��DJ�DJ��DK�DK��DL�DL�RDM�DM�RDN�DN��DO�DO{�DP�DP�RDQ�DQ�DR�DR��DS�DS��DTDT��DU�DU��DVDV�DWDW��DX�DX�RDYRDY�RDZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_�D`�D`��Da�Da��DbRDb��DcRDc��DdRDd��DeRDe�RDf�Df��DgRDg��Dh�Dh��DiRDi�RDjRDj�RDkRDk��Dl�Dl�RDmRDm�RDn�Dn��DoRDo��Dp�Dp�RDq�Dq�RDq��Dr��DsRDs��DtRDt��DuRDu��DvRDv��DwRDw��Dy�{D�[31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ffA�ffA�ffA�dZA�bNA�dZA�hsA�ffA�hsA�hsA�hsA�jA�n�A�t�A�r�A�r�A�v�A�|�A�|�A�v�A�v�A�x�A�t�A�t�A�v�A�|�A�~�A�~�AāAāAāAāA�~�AāAāAăAąAąAăAăAĉ7AčPAčPAčPAď\Aď\Ać+A�|�A�l�A�bNA�K�A�+A�A��A��#AþwA�A¼jA�t�A�{A�ffA�/A�p�A�n�A��A�"�A�K�A���A�dZA�(�A��A��A�
=A���A���A���A��FA�|�A���A�1A�C�A�S�A�n�A���A�dZA��9A���A��A�XA�+A���A��FA�G�A�A�A���A��jA��HA��uA�ffA�{A�33A�/A�A�A��A���A���A�+A��A}�AxQ�Ar�uAoS�Akl�AhQ�Ag7LAe�Ac&�A`v�A^�yA[VAU�
AR �AO;dAL�AH  AGAF��AEdZAB�!A@�\A>I�A;�A9`BA8��A8�RA8��A8��A8�\A8�A8  A8  A7��A7+A6�9A5O�A3?}A.1'A,�+A+�PA*ȴA)?}A'�TA%�A$r�A#��A#XA"��A"$�A"{A!�A!t�A �A/A$�A��AAJA�/AE�An�A��A�-A�A��A1A�FA"�A�A&�A�A+A�`AE�A��AdZA�A��Az�A|�A��A5?A?}A	�A��A�/At�A�/Ar�AbAO�A�jA1'AƨA"�A ĜA �A b@��@�{@��D@�-@�G�@���@�v�@�b@���@��;@���@�w@��H@�ff@�@�?}@�@�|�@��H@ᙚ@���@��u@��@߮@�+@�$�@��@ڸR@���@�(�@�b@��
@�;d@�p�@��`@Դ9@�1@Ұ!@љ�@�A�@�  @ϝ�@�-@�1'@˶F@�dZ@��@ɉ7@ȓu@���@ư!@���@�`B@���@�ƨ@��H@���@��/@�C�@���@��@���@�@�Ĝ@�  @�S�@��7@�7L@��@�x�@�A�@��7@�Z@�Q�@���@�v�@�n�@�~�@�v�@���@�I�@�~�@�`B@�G�@��@�%@��j@�r�@� �@���@���@�t�@���@�&�@���@�9X@��@��w@��@�33@��@���@�^5@�5?@�-@��@�x�@�&�@��j@��u@�bN@��
@�dZ@�;d@�
=@��H@���@���@���@��D@� �@��@�dZ@�@���@��7@�&�@��@��/@���@���@��j@���@�z�@�1'@��F@��@���@��P@�|�@�\)@�\)@�S�@�
=@���@�^5@��@��7@�/@��@���@��@��m@��w@�C�@���@�V@�{@��@���@�`B@�/@��`@�z�@���@���@���@���@�K�@�"�@�33@��\@���@�p�@�/@���@�z�@�bN@�A�@�A�@�I�@�I�@�I�@�A�@� �@�ƨ@��P@�dZ@��y@��!@���@���@���@��+@�~�@�ff@�E�@��@���@��#@���@�@��-@�hs@��j@�j@�Q�@�(�@��F@�l�@�;d@�
=@���@�V@�J@�@��T@�7L@��`@�Ĝ@��D@�r�@�Q�@�9X@�(�@��@���@�l�@�;d@�
=@��y@���@�^5@��#@�p�@�&�@���@��@��@�j@��F@�l�@�K�@��y@��R@��\@�n�@�V@�=q@���@�@�hs@�&�@�%@���@�z�@�Z@��@���@�t�@�\)@�C�@�o@�@�ȴ@��R@���@�ff@�V@�-@��T@���@��@�x�@�hs@�`B@�&�@��9@��D@�A�@�b@�;@|�@
=@~ȴ@~5?@}��@{�K@m��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�ffA�ffA�ffA�dZA�bNA�dZA�hsA�ffA�hsA�hsA�hsA�jA�n�A�t�A�r�A�r�A�v�A�|�A�|�A�v�A�v�A�x�A�t�A�t�A�v�A�|�A�~�A�~�AāAāAāAāA�~�AāAāAăAąAąAăAăAĉ7AčPAčPAčPAď\Aď\Ać+A�|�A�l�A�bNA�K�A�+A�A��A��#AþwA�A¼jA�t�A�{A�ffA�/A�p�A�n�A��A�"�A�K�A���A�dZA�(�A��A��A�
=A���A���A���A��FA�|�A���A�1A�C�A�S�A�n�A���A�dZA��9A���A��A�XA�+A���A��FA�G�A�A�A���A��jA��HA��uA�ffA�{A�33A�/A�A�A��A���A���A�+A��A}�AxQ�Ar�uAoS�Akl�AhQ�Ag7LAe�Ac&�A`v�A^�yA[VAU�
AR �AO;dAL�AH  AGAF��AEdZAB�!A@�\A>I�A;�A9`BA8��A8�RA8��A8��A8�\A8�A8  A8  A7��A7+A6�9A5O�A3?}A.1'A,�+A+�PA*ȴA)?}A'�TA%�A$r�A#��A#XA"��A"$�A"{A!�A!t�A �A/A$�A��AAJA�/AE�An�A��A�-A�A��A1A�FA"�A�A&�A�A+A�`AE�A��AdZA�A��Az�A|�A��A5?A?}A	�A��A�/At�A�/Ar�AbAO�A�jA1'AƨA"�A ĜA �A b@��@�{@��D@�-@�G�@���@�v�@�b@���@��;@���@�w@��H@�ff@�@�?}@�@�|�@��H@ᙚ@���@��u@��@߮@�+@�$�@��@ڸR@���@�(�@�b@��
@�;d@�p�@��`@Դ9@�1@Ұ!@љ�@�A�@�  @ϝ�@�-@�1'@˶F@�dZ@��@ɉ7@ȓu@���@ư!@���@�`B@���@�ƨ@��H@���@��/@�C�@���@��@���@�@�Ĝ@�  @�S�@��7@�7L@��@�x�@�A�@��7@�Z@�Q�@���@�v�@�n�@�~�@�v�@���@�I�@�~�@�`B@�G�@��@�%@��j@�r�@� �@���@���@�t�@���@�&�@���@�9X@��@��w@��@�33@��@���@�^5@�5?@�-@��@�x�@�&�@��j@��u@�bN@��
@�dZ@�;d@�
=@��H@���@���@���@��D@� �@��@�dZ@�@���@��7@�&�@��@��/@���@���@��j@���@�z�@�1'@��F@��@���@��P@�|�@�\)@�\)@�S�@�
=@���@�^5@��@��7@�/@��@���@��@��m@��w@�C�@���@�V@�{@��@���@�`B@�/@��`@�z�@���@���@���@���@�K�@�"�@�33@��\@���@�p�@�/@���@�z�@�bN@�A�@�A�@�I�@�I�@�I�@�A�@� �@�ƨ@��P@�dZ@��y@��!@���@���@���@��+@�~�@�ff@�E�@��@���@��#@���@�@��-@�hs@��j@�j@�Q�@�(�@��F@�l�@�;d@�
=@���@�V@�J@�@��T@�7L@��`@�Ĝ@��D@�r�@�Q�@�9X@�(�@��@���@�l�@�;d@�
=@��y@���@�^5@��#@�p�@�&�@���@��@��@�j@��F@�l�@�K�@��y@��R@��\@�n�@�V@�=q@���@�@�hs@�&�@�%@���@�z�@�Z@��@���@�t�@�\)@�C�@�o@�@�ȴ@��R@���@�ff@�V@�-@��T@���@��@�x�@�hs@�`B@�&�@��9@��D@�A�@�b@�;@|�@
=@~ȴ@~5?@}��@{�K@m��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\BbBhBuBuBoBhBhB�B�B$�Bn�B�oB��B��B��B��BȴB�B�BB�B_;B� B��B��B�B�9B�?B�?B�?B�'B�-B�9B�!B��B��B�B�B��B��B�1BXBbB��B��B�JBD�B1B
�mB
�ZB
�RB
�3B
�wB
�wB
�B
�uB
x�B
iyB
dZB
`BB
XB
-B
%�B
\B	�B	��B	��B	�+B	r�B	hsB	]/B	P�B	E�B	;dB	$�B	hB	  B��B�B�)B��B��BȴB�}B�3B�B��B��B��B��B��B��B��B�B�B�B�B�B�B�B��B��B��B�hB�JB�+B�JB�bB�\B�\B�uB�oB�bB��B��B��B�{B�Bx�Bu�Bt�Br�Br�Bs�Bw�B�B�DB�7B�1B�+B�+B�+B�1B�%B�B�B� B�B�%B�1B�7B�7B�7B�DB�PB�\B�bB�oB�hB�bB�hB�bB�\B�VB�VB�VB�VB�PB�VB�\B�VB�PB�\B�VB�VB�hB�hB�oB�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�?B�?B�RB�XB�XB�XB�^B�XB�XB�XB�^B�qB��BŢBĜBĜBƨBƨB��B��B��B��B�B�B�)B�;B�HB�NB�ZB�TB�fB�B�B�B��B��B��B��B	B��B��B	%B	�B	�B	uB	JB	VB	bB	{B	 �B	#�B	%�B	&�B	.B	/B	/B	5?B	6FB	7LB	8RB	9XB	<jB	?}B	@�B	@�B	A�B	C�B	N�B	P�B	R�B	T�B	VB	XB	[#B	]/B	_;B	aHB	bNB	bNB	cTB	ffB	gmB	iyB	jB	iyB	k�B	n�B	q�B	q�B	s�B	u�B	y�B	z�B	y�B	z�B	� B	�B	�B	�%B	�7B	�VB	�oB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�9B	�FB	�RB	�RB	�^B	�wB	�}B	��B	��B	ÖB	ĜB	ĜB	ƨB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�#B	�/B	�/B	�5B	�5B	�;B	�NB	�TB	�TB	�NB	�ZB	�`B	�`B	�fB	�fB	�mB	�mB	�mB	�mB	�sB	�sB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
  B	��B
  B
  B
B
B
B
B
B
B
%B
%B
%B
+B
+B
1B
1B
1B
	7B
	7B
	7B

=B

=B

=B

=B

=B

=B

=B
DB
JB
JB
PB
PB
PB
VB
VB
\B
bB
�B
!b2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\B\BbBhBuBuBoBhBhB�B�B$�Bn�B�oB��B��B��B��BȴB�B�BB�B_;B� B��B��B�B�9B�?B�?B�?B�'B�-B�9B�!B��B��B�B�B��B��B�1BXBbB��B��B�JBD�B1B
�mB
�ZB
�RB
�3B
�wB
�wB
�B
�uB
x�B
iyB
dZB
`BB
XB
-B
%�B
\B	�B	��B	��B	�+B	r�B	hsB	]/B	P�B	E�B	;dB	$�B	hB	  B��B�B�)B��B��BȴB�}B�3B�B��B��B��B��B��B��B��B�B�B�B�B�B�B�B��B��B��B�hB�JB�+B�JB�bB�\B�\B�uB�oB�bB��B��B��B�{B�Bx�Bu�Bt�Br�Br�Bs�Bw�B�B�DB�7B�1B�+B�+B�+B�1B�%B�B�B� B�B�%B�1B�7B�7B�7B�DB�PB�\B�bB�oB�hB�bB�hB�bB�\B�VB�VB�VB�VB�PB�VB�\B�VB�PB�\B�VB�VB�hB�hB�oB�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�?B�?B�RB�XB�XB�XB�^B�XB�XB�XB�^B�qB��BŢBĜBĜBƨBƨB��B��B��B��B�B�B�)B�;B�HB�NB�ZB�TB�fB�B�B�B��B��B��B��B	B��B��B	%B	�B	�B	uB	JB	VB	bB	{B	 �B	#�B	%�B	&�B	.B	/B	/B	5?B	6FB	7LB	8RB	9XB	<jB	?}B	@�B	@�B	A�B	C�B	N�B	P�B	R�B	T�B	VB	XB	[#B	]/B	_;B	aHB	bNB	bNB	cTB	ffB	gmB	iyB	jB	iyB	k�B	n�B	q�B	q�B	s�B	u�B	y�B	z�B	y�B	z�B	� B	�B	�B	�%B	�7B	�VB	�oB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�9B	�FB	�RB	�RB	�^B	�wB	�}B	��B	��B	ÖB	ĜB	ĜB	ƨB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�#B	�/B	�/B	�5B	�5B	�;B	�NB	�TB	�TB	�NB	�ZB	�`B	�`B	�fB	�fB	�mB	�mB	�mB	�mB	�sB	�sB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
  B	��B
  B
  B
B
B
B
B
B
B
%B
%B
%B
+B
+B
1B
1B
1B
	7B
	7B
	7B

=B

=B

=B

=B

=B

=B

=B
DB
JB
JB
PB
PB
PB
VB
VB
\B
bB
�B
!b2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.13 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190616                              AO  ARCAADJP                                                                    20181005190616    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190616  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190616  QCF$                G�O�G�O�G�O�8000            