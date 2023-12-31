CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:40Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @P   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  B   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  J�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Q�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  X`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Z   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  `�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  b�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pp   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  r(   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  z�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �L   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �\   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �`   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �p   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �t   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �x   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �|Argo profile    3.1 1.2 19500101000000  20181005191740  20181005191740  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @���ն~�1   @���`�@5/��v��d�V�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   B   @���@���A   A!��A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B��B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B�33B�  B�  B�33B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  C   C  C�fC�fC�fC
  C  C  C�C  C  C  C  C  C  C  C�fC"  C$  C&  C(�C*  C,  C.  C0  C2  C4  C6  C8  C:  C<�C>  C?�fCA�fCC�fCF  CH�CJ  CK�fCN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Ck�fCm�fCp  Cr�Ct  Cv  Cx  Cz�C|  C~  C�  C��3C�  C��C�  C�  C�  C��C��C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C��C��C��C��3C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C��3C�  C�  C��3C��C��C��C�  C�  C�  C��3C�  C��C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C��fC�  C�  C��C��C��3C��3C��3C�  C��3C�  C�  C�  C��C��C�  C��C��C��C��C��C�  C��3C��C�  C��3C�  C�  C�  C��C�  C��3C�  C�  C��3C�  C��3C�  C��C��C�  C��3C�  C�  C�  C��C�  C�  C��3C�  C��C�  C��3C�  C�  C�  C��3C��C�  C�  C�  C�  C�  C�  C�  D   D �fDfD� D  D� D��D� D  Dy�D  Dy�D��D� D  D� DfD��D	  D	s3D	��D
y�D
��D��D  D� DfD� D  D� D  D� D��Dy�D��D�fDfD�fD��D� DfDy�D  D�fD  D� D  D� D  D� DfD� D��D� D  D� D��Dy�DfDy�D  D�fD  D� D fD y�D!  D!� D"  D"y�D#  D#� D/y�D0  D0y�D1  D1�fD2  D2� D3  D3� D3�3D4y�D4��D5y�D5��D6� D7  D7�fD8fD8y�D8��D9�fD:fD:� D:��D;y�D<fD<�fD<��D=� D>  D>� D?fD?� D@fD@�fDAfDA�fDBfDB�fDB��DCs3DD  DD�fDE  DEy�DF  DF� DG  DG�fDHfDH�fDH��DI� DJ  DJ�fDKfDK�fDL  DLy�DMfDMy�DNfDN� DOfDO� DPfDP� DP�3DQy�DQ��DRs3DS  DS��DTfDTy�DT��DU�fDV  DV�fDWfDW� DX  DX� DY  DY�fDY��DZ�fD[  D[�fD\  D\� D]fD]�fD^  D^� D_  D_y�D`  D`� Da  Da�fDb�Db� Db��Dc�fDd  Dd�fDe  Dey�Df  Df� Dg  Dyu�D�J�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��A(�A%AD(�Ad(�A�{A�{A�{A�{A�{A�{A�{A�{B
=B	
=B
=B��B!
=B)
=B1
=B9
=BA
=BI
=BQ
=BY
=Ba
=Bi
=Bq
=By
=B��RB��RB��B��B��RB��B��B��RB��RB��B��B��B��B��B��B��B��BąBȅB̅BЅBԅB؅B܅B��B�B�RB�B��B�B��B��C B�CB�C(�C(�C(�C
B�CB�CB�C\)CB�CB�CB�CB�CB�CB�CB�C (�C"B�C$B�C&B�C(\)C*B�C,B�C.B�C0B�C2B�C4B�C6B�C8B�C:B�C<\)C>B�C@(�CB(�CD(�CFB�CH\)CJB�CL(�CNB�CPB�CRB�CTB�CVB�CXB�CZB�C\B�C^B�C`B�CbB�CdB�CfB�ChB�CjB�Cl(�Cn(�CpB�Cr\)CtB�CvB�CxB�Cz\)C|B�C~B�C�!HC�{C�!HC�.C�!HC�!HC�!HC�.C�.C�!HC�!HC�!HC�.C�!HC�!HC�.C�!HC�!HC�!HC�!HC�.C�.C�.C�{C�{C�{C�{C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�{C�{C�{C�{C�!HC�!HC�{C�.C�.C�.C�!HC�!HC�!HC�{C�!HC�.C�!HC�!HC�{C�!HC�.C�!HC�!HC�!HC�!HC�!HC�!HC��C�!HC�!HC�.C�.C�{C�{C�{C�!HC�{C�!HC�!HC�!HC�.C�.C�!HC�.C�.C�.C�.C�.C�!HC�{C�.C�!HC�{C�!HC�!HC�!HC�.C�!HC�{C�!HC�!HC�{C�!HC�{C�!HC�.C�.C�!HC�{C�!HC�!HC�!HC�.C�!HC�!HC�{C�!HC�.C�!HC�{C�!HC�!HC�!HC�{C�.C�!HC�!HC�!HC�!HC�!HC�!HC�!HD �D �
D
D��D�D��D
>D��D�D�>D�D�>D
>D��D�D��D
D�qD	�D	��D

>D
�>D
>D�qD�D��D
D��D�D��D�D��D
>D�>D
>D�
D
D�
D
>D��D
D�>D�D�
D�D��D�D��D�D��D
D��D
>D��D�D��D
>D�>D
D�>D�D�
D�D��D 
D �>D!�D!��D"�D"�>D#�D#��D/�>D0�D0�>D1�D1�
D2�D2��D3�D3��D4�D4�>D5
>D5�>D6
>D6��D7�D7�
D8
D8�>D9
>D9�
D:
D:��D;
>D;�>D<
D<�
D=
>D=��D>�D>��D?
D?��D@
D@�
DA
DA�
DB
DB�
DC
>DC��DD�DD�
DE�DE�>DF�DF��DG�DG�
DH
DH�
DI
>DI��DJ�DJ�
DK
DK�
DL�DL�>DM
DM�>DN
DN��DO
DO��DP
DP��DQ�DQ�>DR
>DR��DS�DS�qDT
DT�>DU
>DU�
DV�DV�
DW
DW��DX�DX��DY�DY�
DZ
>DZ�
D[�D[�
D\�D\��D]
D]�
D^�D^��D_�D_�>D`�D`��Da�Da�
DbqDb��Dc
>Dc�
Dd�Dd�
De�De�>Df�Df��Dg�Dy�gD�S311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�x�A�~�AʅAʅAʅAʉ7Aʉ7AʑhAʓuAʓuAʓuAʗ�Aʙ�Aʝ�Aʗ�AʃA�1'Aɝ�AȼjA���A���A�A�(�A�bA�1A���A���A��A��
AƼjAƩ�Aơ�AƟ�AƝ�AƝ�AƗ�Aƕ�AƑhAƍPAƉ7AƃA�`BAŶFA�%AļjA�\)A°!A��#A��A��A�7LA��A�(�A�dZA�S�A��A�hsA��A��uA��A���A�?}A�t�A�~�A�x�A�=qA�G�A��wA�;dA���A���A��A��yA���A��A��A��uA�A�A���A���A��A�
=A���A��wA�ƨA�  A��HA�ƨA�z�A�
=A�z�A�A��9A���A��A���A�S�A�r�A���A�C�A���A��A���A�S�A�\)A�JA���A�A���A���A��`A��A�O�A���A�p�A���A~z�A{l�AuƨAt�Ar�uAqAp��Ao;dAm��Alz�Ak�^Aix�Ag��Af��Ae/AcdZAb�\Aa"�A_�FA_A^v�A]�A\5?AY��AX�DAUx�ATbNAS��AS`BAS"�ARĜARA�AQt�AQ�AP1ANn�AKS�AI�AHE�AFȴAD��AA��A?G�A=�^A<��A<Q�A;
=A933A6�9A5��A5�7A5p�A5O�A4�`A4E�A3`BA1�
A0�RA/��A/
=A.^5A-t�A,9XA*��A(�jA&�A&(�A%�
A$�/A#l�A"$�A!hsA ȴA z�A Q�A 1A7LAt�A��Az�A�A�A�HA��A��A �A��A%A�hA�jA�A�+A�A�HA�TAK�A�mAv�A�A
z�A	�A��A�hA�TA�9A�AO�A+A�A ��A   @��@�|�@��@�Ĝ@��F@�
=@��+@���@��@�-@�1'@��@�I�@�t�@�;d@�ȴ@���@��@�@�33@��@�9@�1@�P@�n�@�&�@�1'@�o@ޏ\@�E�@��@ݲ-@�G�@��@�V@؋D@�~�@�A�@�1@�"�@ӥ�@ӶF@�;d@��@�^5@ɲ-@���@��@�ƨ@���@�Ĝ@��9@��@�  @��@�~�@��^@�`B@��@��j@��F@��#@�/@��@�A�@��P@�o@��!@�v�@�E�@�x�@���@���@�z�@�bN@�Q�@�b@��@��P@���@���@��D@�A�@��@��9@�9X@��m@�S�@�ȴ@��+@��h@���@��u@�j@��m@�\)@�33@�33@��@�|�@�l�@���@�-@�@��T@���@��^@���@�&�@��@�bN@�I�@� �@���@�+@�5?@��@�@��7@���@��@��u@�1'@�|�@�;d@��y@�^5@�{@��#@���@�Ĝ@�A�@��@���@�;d@��y@���@��@�Q�@�"�@�X@�`B@�p�@�&�@��D@��D@�9X@��@��F@�%@��`@�1'@��y@���@�-@�@���@��T@��^@��^@�@�@��T@�p�@��#@�{@�@���@���@��7@�&�@���@���@���@�bN@���@���@��w@��@��@���@���@��@��!@�~�@�J@�x�@�G�@��@�Ĝ@�1'@�  @�  @��@��@��;@��w@�;d@��@���@�@���@���@�@��-@��-@���@���@�x�@�G�@��@��@x*�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�x�A�~�AʅAʅAʅAʉ7Aʉ7AʑhAʓuAʓuAʓuAʗ�Aʙ�Aʝ�Aʗ�AʃA�1'Aɝ�AȼjA���A���A�A�(�A�bA�1A���A���A��A��
AƼjAƩ�Aơ�AƟ�AƝ�AƝ�AƗ�Aƕ�AƑhAƍPAƉ7AƃA�`BAŶFA�%AļjA�\)A°!A��#A��A��A�7LA��A�(�A�dZA�S�A��A�hsA��A��uA��A���A�?}A�t�A�~�A�x�A�=qA�G�A��wA�;dA���A���A��A��yA���A��A��A��uA�A�A���A���A��A�
=A���A��wA�ƨA�  A��HA�ƨA�z�A�
=A�z�A�A��9A���A��A���A�S�A�r�A���A�C�A���A��A���A�S�A�\)A�JA���A�A���A���A��`A��A�O�A���A�p�A���A~z�A{l�AuƨAt�Ar�uAqAp��Ao;dAm��Alz�Ak�^Aix�Ag��Af��Ae/AcdZAb�\Aa"�A_�FA_A^v�A]�A\5?AY��AX�DAUx�ATbNAS��AS`BAS"�ARĜARA�AQt�AQ�AP1ANn�AKS�AI�AHE�AFȴAD��AA��A?G�A=�^A<��A<Q�A;
=A933A6�9A5��A5�7A5p�A5O�A4�`A4E�A3`BA1�
A0�RA/��A/
=A.^5A-t�A,9XA*��A(�jA&�A&(�A%�
A$�/A#l�A"$�A!hsA ȴA z�A Q�A 1A7LAt�A��Az�A�A�A�HA��A��A �A��A%A�hA�jA�A�+A�A�HA�TAK�A�mAv�A�A
z�A	�A��A�hA�TA�9A�AO�A+A�A ��A   @��@�|�@��@�Ĝ@��F@�
=@��+@���@��@�-@�1'@��@�I�@�t�@�;d@�ȴ@���@��@�@�33@��@�9@�1@�P@�n�@�&�@�1'@�o@ޏ\@�E�@��@ݲ-@�G�@��@�V@؋D@�~�@�A�@�1@�"�@ӥ�@ӶF@�;d@��@�^5@ɲ-@���@��@�ƨ@���@�Ĝ@��9@��@�  @��@�~�@��^@�`B@��@��j@��F@��#@�/@��@�A�@��P@�o@��!@�v�@�E�@�x�@���@���@�z�@�bN@�Q�@�b@��@��P@���@���@��D@�A�@��@��9@�9X@��m@�S�@�ȴ@��+@��h@���@��u@�j@��m@�\)@�33@�33@��@�|�@�l�@���@�-@�@��T@���@��^@���@�&�@��@�bN@�I�@� �@���@�+@�5?@��@�@��7@���@��@��u@�1'@�|�@�;d@��y@�^5@�{@��#@���@�Ĝ@�A�@��@���@�;d@��y@���@��@�Q�@�"�@�X@�`B@�p�@�&�@��D@��D@�9X@��@��F@�%@��`@�1'@��y@���@�-@�@���@��T@��^@��^@�@�@��T@�p�@��#@�{@�@���@���@��7@�&�@���@���@���@�bN@���@���@��w@��@��@���@���@��@��!@�~�@�J@�x�@�G�@��@�Ĝ@�1'@�  @�  @��@��@��;@��w@�;d@��@���@�@���@���@�@��-@��-@���@���@�x�@�G�@��@��@x*�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B\)B[#B[#B[#B[#B\)B]/B^5B_;B_;B`BBcTBgmBk�B}�B�uBȴB�B  B
=BoB�B)�B(�B)�B+B,B,B,B-B-B.B.B-B-B-B.B.B.B1'B5?BI�BiyBr�Bu�Bw�B�\B��B��B�'B�9B�!B��B��B��B��B��B��B�B�3B�wB��B�'B��B��B�bB�1B|�B{�By�Bt�Bk�BbNBR�BG�B9XB/B0!B/B%�B"�B!�B �B�BhB1B%BBB��B�B�B�#B��BƨB�XB�'B��B��B|�BL�B.B49B:^B0!B�BB
�B
�sB
�B
ɺB
ȴB
ĜB
�dB
�B
�hB
~�B
ffB
?}B
2-B
+B
%�B
�B
uB
+B	��B	��B	�B	�ZB	�#B	��B	ŢB	�}B	�LB	�B	��B	��B	��B	��B	�B	|�B	jB	aHB	_;B	^5B	]/B	[#B	W
B	Q�B	N�B	D�B	2-B	�B	�B	�B	�B	PB	B��B��B�B�B�ZB�#B��B��B��B��B��BǮB��B�^B�'B�B��B��B��B��B��B��B��B�{B�uB�oB�hB�\B�\B�PB�DB�7B�1B�B�B}�B|�B|�B{�B{�Bz�By�By�By�By�By�Bw�Bs�Bs�Bq�Bm�Bk�BjBiyBgmBffBdZBdZBcTBaHB_;B\)BZBYBYBYBXBXBW
BT�BVBW
BVBW
BW
BVBXBZBZBW
BT�BT�BT�BT�BT�BT�BT�BS�BR�BR�BQ�BS�BT�BS�BR�BR�BT�BT�BT�BVBVBW
BW
BW
BYBZB_;BaHBdZBs�Bu�Bu�Bv�Bm�Bl�Bn�Bo�Bt�B|�B�B�B�B�1B�=B�DB�bB�oB�uB��B��B��B��B��B��B��B��B�B�B�B�-B�LB�^B�dB�jB�wB��BƨB��B��B��B�BB�B��B	B	+B		7B	JB	\B	bB	{B	�B	�B	�B	�B	!�B	"�B	"�A�VB	N�B	N�B	Q�B	XB	ZB	\)B	]/B	^5B	`BB	dZB	iyB	jB	jB	k�B	p�B	t�B	z�B	{�B	{�B	{�B	{�B	|�B	~�B	}�B	|�B	{�B	{�B	z�B	y�B	{�B	� B	}�B	{�B	{�B	|�B	~�B	~�B	~�B	�B	�JB	�VB	�1B	�7B	�=B	�=B	�=B	�DB	�JB	�JB	�PB	��B	��B	��B	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�?B	�FB	�RB	�XB	�XB	�jB	��B	��B	ÖB	ŢB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�#B	�#B	�#B	�#B	�#B	�/B	�/B	�/B	�;B	�BB	�HB	�HB	�HB	�HB	�HB	�NB	�TB	�TB	�ZB	��B
122222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222224222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222B\)B[#B[#B[#B[#B\)B]/B^5B_;B_;B`BBcTBgmBk�B}�B�uBȴB�B  B
=BoB�B)�B(�B)�B+B,B,B,B-B-B.B.B-B-B-B.B.B.B1'B5?BI�BiyBr�Bu�Bw�B�\B��B��B�'B�9B�!B��B��B��B��B��B��B�B�3B�wB��B�'B��B��B�bB�1B|�B{�By�Bt�Bk�BbNBR�BG�B9XB/B0!B/B%�B"�B!�B �B�BhB1B%BBB��B�B�B�#B��BƨB�XB�'B��B��B|�BL�B.B49B:^B0!B�BB
�B
�sB
�B
ɺB
ȴB
ĜB
�dB
�B
�hB
~�B
ffB
?}B
2-B
+B
%�B
�B
uB
+B	��B	��B	�B	�ZB	�#B	��B	ŢB	�}B	�LB	�B	��B	��B	��B	��B	�B	|�B	jB	aHB	_;B	^5B	]/B	[#B	W
B	Q�B	N�B	D�B	2-B	�B	�B	�B	�B	PB	B��B��B�B�B�ZB�#B��B��B��B��B��BǮB��B�^B�'B�B��B��B��B��B��B��B��B�{B�uB�oB�hB�\B�\B�PB�DB�7B�1B�B�B}�B|�B|�B{�B{�Bz�By�By�By�By�By�Bw�Bs�Bs�Bq�Bm�Bk�BjBiyBgmBffBdZBdZBcTBaHB_;B\)BZBYBYBYBXBXBW
BT�BVBW
BVBW
BW
BVBXBZBZBW
BT�BT�BT�BT�BT�BT�BT�BS�BR�BR�BQ�BS�BT�BS�BR�BR�BT�BT�BT�BVBVBW
BW
BW
BYBZB_;BaHBdZBs�Bu�Bu�Bv�Bm�Bl�Bn�Bo�Bt�B|�B�B�B�B�1B�=B�DB�bB�oB�uB��B��B��B��B��B��B��B��B�B�B�B�-B�LB�^B�dB�jB�wB��BƨB��B��B��B�BB�B��B	B	+B		7B	JB	\B	bB	{B	�B	�B	�B	�B	!�B	"�B	"�A�VB	N�B	N�B	Q�B	XB	ZB	\)B	]/B	^5B	`BB	dZB	iyB	jB	jB	k�B	p�B	t�B	z�B	{�B	{�B	{�B	{�B	|�B	~�B	}�B	|�B	{�B	{�B	z�B	y�B	{�B	� B	}�B	{�B	{�B	|�B	~�B	~�B	~�B	�B	�JB	�VB	�1B	�7B	�=B	�=B	�=B	�DB	�JB	�JB	�PB	��B	��B	��B	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�?B	�FB	�RB	�XB	�XB	�jB	��B	��B	ÖB	ŢB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�#B	�#B	�#B	�#B	�#B	�/B	�/B	�/B	�;B	�BB	�HB	�HB	�HB	�HB	�HB	�NB	�TB	�TB	�ZB	��B
122222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222224222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.26 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191740                              AO  ARCAADJP                                                                    20181005191740    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191740  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191740  QCF$                G�O�G�O�G�O�8000            