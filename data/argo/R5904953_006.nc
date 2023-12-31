CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:06:17Z creation      
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
_FillValue                 �  A<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^|   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  fH   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h<   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �T   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �$   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �(   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �,   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20181005190617  20181005190617  5904953 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6432                            2B  A   APEX                            7467                            062512                          846 @נDH?�*1   @נD�}*@3��O�;d�c����+1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   A   @9��@�  @�  A   A!��AA��A`  A�  A���A�  A�33A�  A�  A���A�  B   B  BffB  B   B(  B0ffB8  B?��BG��BP  BX  B`  Bh  Bp  Bx  B�  B�  B���B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  C   C  C  C  C  C
  C  C�C  C  C  C  C  C  C  C  C   C"  C$  C&�C(  C*  C,  C.  C0  C1�fC4  C6�C8  C9�fC<  C>�C@�CB�CD�CF�CH  CJ  CL  CN  CO�fCQ�fCT  CV  CX�CZ  C\  C^  C`�Cb  Cd�Cf  Cg�fCj  Cl33Cn  Co�fCr  Cs��Cu�fCx  Cz�C|  C}�fC�  C��C��C��C��C��C�  C��C��C��C��3C�  C��C��C��3C��fC�  C��C��C�  C��fC��fC��3C�  C��3C��C��C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C�  C��C�  C�  C�  C�  C�  C�  C�  C��3C��C��3C�  C�  C��C��3C�  C�  C��C�  C��C�  C��C��fC�  C��C��C��C��C��3C��3C�  C�  C�  C��C�  C�  C��C�  C�  C��C��3C�  C�  C��3C��3C�  C��C��C�  C��3C��3C��C��3C��3C�  C��3C�  C��C��C��3C��3C��3C��C��C�  C�  C��3C�  C��C��C��C�  C�  C�  C��C��C��C�  C��3C��3C��fC��fC�  C��C��C�  C��3D � D  D�fD  Dy�D��D� D��D� DfD� D��Dy�D  D� D  D� D	  D	� D	��D
s3D  D��D  D� D�D��D�D�fD��Ds3D  D�fD  Dy�DfD�fD�D� D  D��D  D� DfD�fD�D� D  D� D��Dy�DfD� D��Dy�D�3Ds3D�3Ds3D�3D� D  Ds3D�3D y�D!fD!� D!�3D"� D#fD#� D#��D$� D%fD%��D&  D&y�D'�D'� D(  D(�fD(��D)� D*  D*y�D+fD+� D+�3D,�fD-fD-�fD.�D.� D.�3D/s3D/�3D0s3D0��D1y�D1�3D2s3D2�3D3s3D3�3D4s3D4�3D5s3D5��D6� D7  D7�fD8fD8��D9  D9y�D:  D:� D;fD;��D<  D<y�D<��D=y�D=��D>y�D?  D?�fD@  D@s3DA  DA�fDB�DB�fDC  DC�fDD�DD��DE�DE� DE�3DFy�DF�3DGy�DG��DH� DIfDI� DI��DJ� DK  DKs3DK��DL� DMfDM�fDNfDN� DOfDO� DP  DP� DQ  DQ� DRfDR� DR�3DSy�DTfDT�fDT��DU� DV�DV� DV�3DW� DX  DX� DY  DYy�DY��DZ� D[  D[� D\fD\�fD]fD]�fD^  D^y�D_  D_�fD`fD`�fDafDa��DbfDb�fDc  Dc��Dd�Dd��DefDe� De��Dfs3Df��Dg� DhfDh��Di  Dis3Di�3Djs3Dj�3Dky�Dl  Dl�fDm  Dm�fDnfDny�Dn�3Dos3Do��Dp� Dq  Dq� Dr  Dr�fDsfDss3Ds�3Dts3Dt�3Du� DvfDv�fDw�Dw��Dw� Dy��D�;�D�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @A�@�(�@�(�A{A#�AC�Ab{A�
=A��
A�
=A�=pA�
=A�
=A��
A�
=B �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�B�B�B�B�\B�\B�\B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�\B�B�B�B�B�B�B�B�B�u�B�B�B�\B�B�B�B�B�B�B�B�B�B�B�B�B�u�B�u�B�B�B�B�C !HC!HC!HC!HC!HC
!HC!HC:�C!HC!HC!HC!HC!HC!HC!HC!HC !HC"!HC$!HC&:�C(!HC*!HC,!HC.!HC0!HC2�C4!HC6:�C8!HC:�C<!HC>:�C@:�CB:�CD:�CF:�CH!HCJ!HCL!HCN!HCP�CR�CT!HCV!HCX:�CZ!HC\!HC^!HC`:�Cb!HCd:�Cf!HCh�Cj!HClT{Cn!HCp�Cr!HCs�Cv�Cx!HCz:�C|!HC~�C��C�qC�qC�qC�*>C�qC��C�qC�*>C�*>C��C��C�*>C�*>C��C��
C��C�qC�qC��C��
C��
C��C��C��C�qC�*>C�qC��C��C��C��C��C��C��C��C��C��C��C��C�qC��C�qC��C��C��C��C��C��C��C��C�qC��C��C��C�qC��C��C��C�qC��C�qC��C�qC��
C��C�qC�*>C�*>C�qC��C��C��C��C��C�qC��C��C�qC��C��C�qC��C��C��C��C��C��C�qC�qC��C��C��C�qC��C��C��C��C��C�*>C�qC��C��C��C�qC�qC��C��C��C��C�*>C�qC�qC��C��C��C�qC�qC�qC��C��C��C��
C��
C��C�qC�qC��D �D �RDRD��DRD��D�D�RD�D�RD�D�RD�D��DRD�RDRD�RD	RD	�RD
�D
{�DRD�DRD�RDD�DD��D�D{�DRD��DRD��D�D��DD�RDRD�DRD�RD�D��DD�RDRD�RD�D��D�D�RD�D��D��D{�D��D{�D��D�RDRD{�D��D ��D!�D!�RD!��D"�RD#�D#�RD$�D$�RD%�D%�D&RD&��D'D'�RD(RD(��D)�D)�RD*RD*��D+�D+�RD+��D,��D-�D-��D.D.�RD.��D/{�D/��D0{�D1�D1��D1��D2{�D2��D3{�D3��D4{�D4��D5{�D6�D6�RD7RD7��D8�D8�D9RD9��D:RD:�RD;�D;�D<RD<��D=�D=��D>�D>��D?RD?��D@RD@{�DARDA��DBDB��DCRDC��DDDD�DEDE�RDE��DF��DF��DG��DH�DH�RDI�DI�RDJ�DJ�RDKRDK{�DL�DL�RDM�DM��DN�DN�RDO�DO�RDPRDP�RDQRDQ�RDR�DR�RDR��DS��DT�DT��DU�DU�RDVDV�RDV��DW�RDXRDX�RDYRDY��DZ�DZ�RD[RD[�RD\�D\��D]�D]��D^RD^��D_RD_��D`�D`��Da�Da�Db�Db��DcRDc�DdDd�De�De�RDf�Df{�Dg�Dg�RDh�Dh�DiRDi{�Di��Dj{�Dj��Dk��DlRDl��DmRDm��Dn�Dn��Dn��Do{�Dp�Dp�RDqRDq�RDrRDr��Ds�Ds{�Ds��Dt{�Dt��Du�RDv�Dv��DwDw�Dw�RDy�>D�?�D�S�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�Q�A��A�x�A���AȶFAȩ�A�^5A�G�A�;dA��A���AǾwA�x�A�ZA�C�A� �A��A�%A�ĜAơ�A�dZA�1A��A��;A��A��mA���AŮAŗ�AōPA�dZA�C�A�A�A�C�A�O�A�K�A�E�A�+A�"�A�{A�JA��A��TA��HA���A�ȴA�hsA�t�A�~�A�5?AÓuAÁA�XA�{A�{A�
=A+A�"�A���A�Q�A� �A���A�|�A��
A�?}A��A�=qA���A���A��hA�A�7LA�"�A��wA���A�;dA���A���A��A�x�A��A��mA�p�A��A�VA�p�A�jA���A��A�A��A���A�ZA�O�A��RA�(�A�`BA�ZA���A�VA�$�A�%A���A��yA��`A�  A�
=A���A~=qA}/A|�A|��A{|�Aw
=AuC�Ap�Ah�+AfVAe�Ae��Ae`BAd��Ab{A^��A]|�A]��A]�A^-A]"�A[|�AZ$�AY�AY��AY
=AUx�AR1'AQ��APJANQ�ALA�AJ��AHffAGt�AG�AG+AE�ADbNAB��A?��A<�+A9��A6�RA5�-A3t�A0�/A/�hA-A*{A't�A%�
A$^5A"�HA!��A!��A!ƨA!��A!G�A��A�A�`A�`A��A��A��A;dA�A�A��A;dA�A��AZAA�FAĜA�-A7LA�9A��A~�AVA��AoA��A�AI�A/A �@��R@�1'@�%@���@��u@��;@�!@�{@�^@���@�v�@���@�D@�1'@�  @�ƨ@�^5@陚@�@�&�@�\)@�hs@�9X@�"�@���@���@�1'@١�@���@ؓu@ץ�@֏\@ղ-@�z�@�l�@�5?@�hs@Ѓ@θR@��T@���@�t�@˕�@�|�@�K�@�|�@�dZ@�K�@���@ɡ�@��
@�=q@�hs@���@°!@��^@��u@�Z@�b@�;d@��\@�J@���@��@��m@��@���@��@�`B@���@��u@��u@���@�r�@�I�@���@�ƨ@��P@�\)@�;d@��@�~�@��-@��/@�Z@���@��P@���@�V@��@��@�@�/@���@�(�@�33@���@��+@�-@��-@�X@�V@��j@�A�@��m@��F@���@�$�@���@��@�5?@��@���@���@��7@�O�@���@��@���@���@�Ĝ@��@�  @�
=@�~�@�ff@�M�@�J@��#@���@���@�x�@�x�@�hs@�hs@�x�@�?}@�%@��@�A�@�Z@�z�@��D@�j@�9X@���@�n�@�p�@�7L@�&�@�&�@��@��@�V@�V@���@��/@���@�Ĝ@���@��u@��@��@��@�z�@�z�@�z�@�r�@�bN@��@���@�|�@�t�@�t�@�S�@�K�@�+@�"�@��@���@�v�@�-@��@��#@��#@��#@���@���@�X@��`@�Ĝ@��9@�z�@�I�@�1@��@��7@���@��@�z�@�j@��D@��u@�1@�33@�
=@��H@��R@���@��\@�~�@��T@�?}@���@��@���@��@�;d@�
=@��y@��@���@��\@�{@���@���@�p�@�O�@��@�%@��@��@��7@��7@��@�O�@�&�@��@��@���@�r�@���@�l�@��!@��@���@���@�x�@�`B@�hs@�V@���@��`@�z�@� �@���@�;d@�~�@���@�/@���@���@���@��@�ƨ@��@��P@�33@��+@�V@�5?@�$�@��@���@�x�@�X@�?}@�/@�&�@���@��@��@�t�@�o@��!@�~�@�M�@�M�@�E�@��@���@���@��h@�G�@��@���@���@��@�3�@qVm@]��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�Q�A��A�x�A���AȶFAȩ�A�^5A�G�A�;dA��A���AǾwA�x�A�ZA�C�A� �A��A�%A�ĜAơ�A�dZA�1A��A��;A��A��mA���AŮAŗ�AōPA�dZA�C�A�A�A�C�A�O�A�K�A�E�A�+A�"�A�{A�JA��A��TA��HA���A�ȴA�hsA�t�A�~�A�5?AÓuAÁA�XA�{A�{A�
=A+A�"�A���A�Q�A� �A���A�|�A��
A�?}A��A�=qA���A���A��hA�A�7LA�"�A��wA���A�;dA���A���A��A�x�A��A��mA�p�A��A�VA�p�A�jA���A��A�A��A���A�ZA�O�A��RA�(�A�`BA�ZA���A�VA�$�A�%A���A��yA��`A�  A�
=A���A~=qA}/A|�A|��A{|�Aw
=AuC�Ap�Ah�+AfVAe�Ae��Ae`BAd��Ab{A^��A]|�A]��A]�A^-A]"�A[|�AZ$�AY�AY��AY
=AUx�AR1'AQ��APJANQ�ALA�AJ��AHffAGt�AG�AG+AE�ADbNAB��A?��A<�+A9��A6�RA5�-A3t�A0�/A/�hA-A*{A't�A%�
A$^5A"�HA!��A!��A!ƨA!��A!G�A��A�A�`A�`A��A��A��A;dA�A�A��A;dA�A��AZAA�FAĜA�-A7LA�9A��A~�AVA��AoA��A�AI�A/A �@��R@�1'@�%@���@��u@��;@�!@�{@�^@���@�v�@���@�D@�1'@�  @�ƨ@�^5@陚@�@�&�@�\)@�hs@�9X@�"�@���@���@�1'@١�@���@ؓu@ץ�@֏\@ղ-@�z�@�l�@�5?@�hs@Ѓ@θR@��T@���@�t�@˕�@�|�@�K�@�|�@�dZ@�K�@���@ɡ�@��
@�=q@�hs@���@°!@��^@��u@�Z@�b@�;d@��\@�J@���@��@��m@��@���@��@�`B@���@��u@��u@���@�r�@�I�@���@�ƨ@��P@�\)@�;d@��@�~�@��-@��/@�Z@���@��P@���@�V@��@��@�@�/@���@�(�@�33@���@��+@�-@��-@�X@�V@��j@�A�@��m@��F@���@�$�@���@��@�5?@��@���@���@��7@�O�@���@��@���@���@�Ĝ@��@�  @�
=@�~�@�ff@�M�@�J@��#@���@���@�x�@�x�@�hs@�hs@�x�@�?}@�%@��@�A�@�Z@�z�@��D@�j@�9X@���@�n�@�p�@�7L@�&�@�&�@��@��@�V@�V@���@��/@���@�Ĝ@���@��u@��@��@��@�z�@�z�@�z�@�r�@�bN@��@���@�|�@�t�@�t�@�S�@�K�@�+@�"�@��@���@�v�@�-@��@��#@��#@��#@���@���@�X@��`@�Ĝ@��9@�z�@�I�@�1@��@��7@���@��@�z�@�j@��D@��u@�1@�33@�
=@��H@��R@���@��\@�~�@��T@�?}@���@��@���@��@�;d@�
=@��y@��@���@��\@�{@���@���@�p�@�O�@��@�%@��@��@��7@��7@��@�O�@�&�@��@��@���@�r�@���@�l�@��!@��@���@���@�x�@�`B@�hs@�V@���@��`@�z�@� �@���@�;d@�~�@���@�/@���@���@���@��@�ƨ@��@��P@�33@��+@�V@�5?@�$�@��@���@�x�@�X@�?}@�/@�&�@���@��@��@�t�@�o@��!@�~�@�M�@�M�@�E�@��@���@���@��h@�G�@��@���@���@��@�3�@qVm@]��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BF�BD�BF�BE�BL�BS�BXBZB[#B[#BZBYBVBVB[#BaHBcTBe`BffBe`BgmBe`BdZBe`Bl�Bn�Bp�Bn�Bl�Bl�Bn�Bo�Bu�Bz�B�B�JB�bB�hB�oB�uB�{B��B��B��B��B��BȴB�B��BoB(�B+B2-BG�BI�BM�BYB^5BgmBk�Bk�Bl�Bk�BiyBjBl�Bm�Bk�Bu�B�+B��B�{B��B�wB�B�HB�NB�#B�B��B�LB��B��B��B��B��B�PBu�B]/B2-BPB��B�5BȴB�VBo�BL�B'�B
��B
��B
��B
�B
�fB
�B
r�B
iyB
`BB
�B	��B	��B	��B	�B	�fB	��B	�wB	��B	�B	y�B	w�B	s�B	n�B	dZB	B�B	�B	�B	�B	"�B	)�B	#�B	�B	�B	�B	%�B	#�B		7B��B��B�B�B�B�TBǮB��BɺB��B�BɺBɺB��B�?B�!B��B��B��B��B��B��B�B�B�-BĜB��B��B�B�B�B�B�
B�#B�5B�5B�;B�;B�/B�B��B�wB�}B��B�)B�#B�
B��B��BƨBBŢBŢBĜB��B�LB��B��B��B��B��B��B��B�!B�RB�qBBÖBĜBĜBĜBĜBB��B�}B�}B��B��B��BBÖBÖBBĜBÖBB��B�wB�dB�dB�dB�qB��BBĜBŢBɺB��B��B��B��B��B��B��B�B�5B�BB�NB�`B�fB�fB�mB�sB�sB�mB�fB�mB�B�B��B��B��B��B��B��B��B	B	1B		7B	JB	VB	hB	�B	�B	�B	�B	"�B	#�B	%�B	'�B	)�B	,B	,B	-B	/B	2-B	6FB	8RB	:^B	;dB	?}B	D�B	E�B	F�B	G�B	G�B	G�B	I�B	K�B	L�B	O�B	T�B	[#B	\)B	]/B	]/B	^5B	^5B	_;B	`BB	aHB	cTB	e`B	ffB	hsB	jB	jB	k�B	l�B	o�B	r�B	t�B	u�B	u�B	t�B	v�B	z�B	� B	�B	�B	�=B	�\B	�\B	�\B	�VB	�VB	�VB	�VB	�bB	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�3B	�FB	�RB	�RB	�RB	�RB	�^B	�jB	�wB	��B	��B	B	ÖB	ÖB	ĜB	ĜB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	ɺB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�/B	�;B	�HB	�NB	�TB	�`B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
1B
1B
1B
1B
	7B
	7B

=B

=B

=B

=B
DB
JB
xB
 'B
,�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 BF�BD�BF�BE�BL�BS�BXBZB[#B[#BZBYBVBVB[#BaHBcTBe`BffBe`BgmBe`BdZBe`Bl�Bn�Bp�Bn�Bl�Bl�Bn�Bo�Bu�Bz�B�B�JB�bB�hB�oB�uB�{B��B��B��B��B��BȴB�B��BoB(�B+B2-BG�BI�BM�BYB^5BgmBk�Bk�Bl�Bk�BiyBjBl�Bm�Bk�Bu�B�+B��B�{B��B�wB�B�HB�NB�#B�B��B�LB��B��B��B��B��B�PBu�B]/B2-BPB��B�5BȴB�VBo�BL�B'�B
��B
��B
��B
�B
�fB
�B
r�B
iyB
`BB
�B	��B	��B	��B	�B	�fB	��B	�wB	��B	�B	y�B	w�B	s�B	n�B	dZB	B�B	�B	�B	�B	"�B	)�B	#�B	�B	�B	�B	%�B	#�B		7B��B��B�B�B�B�TBǮB��BɺB��B�BɺBɺB��B�?B�!B��B��B��B��B��B��B�B�B�-BĜB��B��B�B�B�B�B�
B�#B�5B�5B�;B�;B�/B�B��B�wB�}B��B�)B�#B�
B��B��BƨBBŢBŢBĜB��B�LB��B��B��B��B��B��B��B�!B�RB�qBBÖBĜBĜBĜBĜBB��B�}B�}B��B��B��BBÖBÖBBĜBÖBB��B�wB�dB�dB�dB�qB��BBĜBŢBɺB��B��B��B��B��B��B��B�B�5B�BB�NB�`B�fB�fB�mB�sB�sB�mB�fB�mB�B�B��B��B��B��B��B��B��B	B	1B		7B	JB	VB	hB	�B	�B	�B	�B	"�B	#�B	%�B	'�B	)�B	,B	,B	-B	/B	2-B	6FB	8RB	:^B	;dB	?}B	D�B	E�B	F�B	G�B	G�B	G�B	I�B	K�B	L�B	O�B	T�B	[#B	\)B	]/B	]/B	^5B	^5B	_;B	`BB	aHB	cTB	e`B	ffB	hsB	jB	jB	k�B	l�B	o�B	r�B	t�B	u�B	u�B	t�B	v�B	z�B	� B	�B	�B	�=B	�\B	�\B	�\B	�VB	�VB	�VB	�VB	�bB	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�3B	�FB	�RB	�RB	�RB	�RB	�^B	�jB	�wB	��B	��B	B	ÖB	ÖB	ĜB	ĜB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	ɺB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�/B	�;B	�HB	�NB	�TB	�`B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
1B
1B
1B
1B
	7B
	7B

=B

=B

=B

=B
DB
JB
xB
 'B
,�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.13 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190617                              AO  ARCAADJP                                                                    20181005190617    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190617  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190617  QCF$                G�O�G�O�G�O�8000            