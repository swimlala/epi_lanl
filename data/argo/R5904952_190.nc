CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:48Z creation      
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
resolution        =���   axis      Z        X  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X  K�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  S(   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  \X   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  c�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  e�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  l�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t8   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  v   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  }h   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  @   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20181005190548  20181005190548  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @�����v�1   @����B�@1�^5?|��c�
=p��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�33A�33A�33B   BffBffB  B   B(  B0  B8  B@  BH  BP  BX  B`  Bg��Bp  Bx  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B���B���B���B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�33B�  B���B�  B���C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C1�fC4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR�CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C��C��C�  C�  C��C��C��C�  C�  C��C�  C�  C��C��C�  C�  C��C��C��C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C��3C��3C�  C�  C��3C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D��D� D  Dy�D  D� D	  D	� D
  D
� DfD� D��D� D  D� D��D� D  D� D  D� D  D�fDfD� D  D� D  D� D  D� D  D� D��D� D  D� D��Dy�D  D� D  D� D��D� D  D� D  D� D  D� D��D y�D!  D!�fD"fD"� D#  D#� D$  D$� D%  D%� D&  D&� D&��D'� D(  D(� D)  D)� D)��D*� D+  D+� D+��D,� D-  D-�fD.  D.� D/  D/� D0  D0� D1fD1�fD2fD2� D2��D3� D4  D4� D5  D5y�D5��D6� D7  D7� D8fD8�fD9fD9�fD:fD:� D:��D;y�D;��D<� D=  D=� D=��D>y�D?  D?� D@  D@y�D@��DA� DB  DB� DC  DC� DD  DDy�DE  DE�fDF  DF� DG  DG� DH  DHy�DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQy�DR  DR�fDS  DS� DTfDT�fDU  DU� DV  DV� DW  DWy�DX  DX� DY  DY�fDZ  DZ�fD[fD[� D\  D\�fD]fD]�fD^fD^�fD_  D_� D`  D`� Da  Da� Db  Db� Dc  Dcy�Dd  Dd� Dd��De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dyx�D�@�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @;�@�(�@�(�A{A"{AB{Ab{A�
=A�
=A�
=A�
=A�
=A�=pA�=pA�=pB �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�B�B�B�B�B�B�B�B�B�B�u�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�\B�\B�\B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�u�B�B�B�B�B�u�B�B�B�\B�B�B�\C !HC!HC!HC!HC!HC
!HC!HC!HC!HC!HC!HC!HC!HC!HC!HC!HC !HC"!HC$!HC&!HC(!HC*!HC,!HC.!HC0!HC2�C4!HC6!HC8!HC:!HC<!HC>!HC@!HCB!HCD!HCF!HCH!HCJ!HCL!HCN!HCP!HCR:�CT!HCV!HCX!HCZ!HC\!HC^!HC`!HCb!HCd!HCf!HCh!HCj!HCl!HCn!HCp!HCr!HCt!HCv!HCx!HCz!HC|!HC~!HC�qC�qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�qC��C��C��C��C��C�qC��C��C��C��C��C��C��C��C��C��C��C�qC��C��C��C�qC�qC��C��C�qC�qC�qC��C��C�qC��C��C�qC�qC��C��C�qC�qC�qC�qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�qC�qC��C��C��C��C��C��C��C��C��C�qC��C��C�qC��C��C��C��C��C��C��C��C�qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D RD �RDRD�RDRD�RDRD�RDRD�RDRD�RD�D�RDRD��DRD�RD	RD	�RD
RD
�RD�D�RD�D�RDRD�RD�D�RDRD�RDRD�RDRD��D�D�RDRD�RDRD�RDRD�RDRD�RD�D�RDRD�RD�D��DRD�RDRD�RD�D�RDRD�RDRD�RDRD�RD �D ��D!RD!��D"�D"�RD#RD#�RD$RD$�RD%RD%�RD&RD&�RD'�D'�RD(RD(�RD)RD)�RD*�D*�RD+RD+�RD,�D,�RD-RD-��D.RD.�RD/RD/�RD0RD0�RD1�D1��D2�D2�RD3�D3�RD4RD4�RD5RD5��D6�D6�RD7RD7�RD8�D8��D9�D9��D:�D:�RD;�D;��D<�D<�RD=RD=�RD>�D>��D?RD?�RD@RD@��DA�DA�RDBRDB�RDCRDC�RDDRDD��DERDE��DFRDF�RDGRDG�RDHRDH��DIRDI�RDJRDJ�RDKRDK�RDLRDL�RDMRDM�RDNRDN�RDORDO�RDPRDP�RDQRDQ��DRRDR��DSRDS�RDT�DT��DURDU�RDVRDV�RDWRDW��DXRDX�RDYRDY��DZRDZ��D[�D[�RD\RD\��D]�D]��D^�D^��D_RD_�RD`RD`�RDaRDa�RDbRDb�RDcRDc��DdRDd�RDe�De�RDfRDf�RDgRDg�RDhRDh�RDiRDi�RDy�HD�ED���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�XA�XA�^5A�`BA�bNA�`BA�`BA�ffA�jA�n�A�jA�Q�A�VA�hsAɾwAȍPA��A���AǺ^A�?}A���A���A���A���AƼjAƺ^Aƺ^A���A��yA��
AƮA�n�A�hsAƣ�A�JA���A��A�ƨA�ĜAžwAŰ!Aś�A�C�A�?}A�z�A��#A�K�A��A��
A���A��/A���A���A�A�A�A���A��yA���A��mA�  A�`BA��A�O�A��A�XA��yA��A�bNA�33A�33A�$�A�33A�ffA�`BA��jA�jA�t�A�^5A�  A�ZA���A��A� �A�^5A�$�A�(�A��\A���A���A��DA�XA���A�$�A���A�\)A�M�A��TA�n�A��A�jA�dZA�ȴA���A�Q�A��hA�\)A��wA��RA��A�  A�M�A���A�oA���A�jA�(�A���A�
=A��A|ZAz(�Au�Ap1An�An{Am"�Ag�Ad��A`jAZffAV��ATbNAR��AO��AL�AI%AG��AD��AA��A=�A9�
A8z�A6��A3�A2��A1�A0ZA.�A-��A-x�A-p�A,�yA+��A+�A);dA'dZA&�A%�A$1A#�A"��A Q�A�A1'A�^A��AbNA�7Ax�Al�A`BAoA�Av�A�TA��AVA�A�A{Al�A1A=qAƨA�hAhsA�`A�7A�jA�FA
��A	�A�AA�FA �A|�Al�A;dAv�A �@�-@���@�M�@�%@��
@��@�X@�o@�7L@�9@�Q�@�K�@�@�G�@�
=@��@�@���@��#@�9X@���@�p�@�?}@�/@�j@�;d@އ+@�E�@�{@�`B@��`@���@�Ĝ@ܣ�@�z�@��;@ە�@�o@�=q@��@��`@��
@�C�@�=q@��T@ղ-@Չ7@�p�@�?}@�  @�=q@Ѻ^@�X@�&�@���@� �@ϥ�@ΰ!@�-@���@́@�?}@�1'@�~�@���@�7L@ȓu@�  @�+@��@�M�@Ł@��@���@ě�@�1@å�@ÍP@�33@���@�-@���@�?}@��@��
@��@���@��@��@���@���@�n�@�J@��`@�A�@��
@�S�@�
=@��@��@�ȴ@���@�M�@�/@��@�%@��j@��D@�r�@�9X@��m@�K�@��H@�
=@�"�@��H@�n�@���@���@���@���@�Ĝ@�bN@��@�l�@�o@�E�@��@��@�hs@�V@���@�r�@�  @��@�;d@�dZ@��@�{@��T@��T@��h@��@���@��H@�ȴ@���@��\@��\@��+@��\@���@�ff@�$�@��-@�&�@��@�A�@��@��w@��@�;d@��@��R@��+@�ff@�-@�@���@��h@��7@�x�@�x�@�?}@���@�Q�@�9X@��;@��@�"�@��@��R@��+@�^5@�J@�7L@���@��@�1@���@�dZ@�"�@��\@�=q@�@��h@���@���@�z�@�I�@�1'@��@�ƨ@�t�@�"�@���@�-@��T@���@��-@��7@�hs@�/@��@�Ĝ@���@�1'@��w@���@���@��P@�t�@�\)@�S�@�;d@��@�@��y@���@�$�@���@���@��@�x�@�hs@�`B@�`B@�X@�&�@���@��`@��`@��/@���@���@� �@�ƨ@��P@�l�@�C�@�33@�"�@�o@��H@��+@�@��h@�?}@�%@���@�Ĝ@�(�@�1@���@��@���@~kQ@l��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�XA�XA�^5A�`BA�bNA�`BA�`BA�ffA�jA�n�A�jA�Q�A�VA�hsAɾwAȍPA��A���AǺ^A�?}A���A���A���A���AƼjAƺ^Aƺ^A���A��yA��
AƮA�n�A�hsAƣ�A�JA���A��A�ƨA�ĜAžwAŰ!Aś�A�C�A�?}A�z�A��#A�K�A��A��
A���A��/A���A���A�A�A�A���A��yA���A��mA�  A�`BA��A�O�A��A�XA��yA��A�bNA�33A�33A�$�A�33A�ffA�`BA��jA�jA�t�A�^5A�  A�ZA���A��A� �A�^5A�$�A�(�A��\A���A���A��DA�XA���A�$�A���A�\)A�M�A��TA�n�A��A�jA�dZA�ȴA���A�Q�A��hA�\)A��wA��RA��A�  A�M�A���A�oA���A�jA�(�A���A�
=A��A|ZAz(�Au�Ap1An�An{Am"�Ag�Ad��A`jAZffAV��ATbNAR��AO��AL�AI%AG��AD��AA��A=�A9�
A8z�A6��A3�A2��A1�A0ZA.�A-��A-x�A-p�A,�yA+��A+�A);dA'dZA&�A%�A$1A#�A"��A Q�A�A1'A�^A��AbNA�7Ax�Al�A`BAoA�Av�A�TA��AVA�A�A{Al�A1A=qAƨA�hAhsA�`A�7A�jA�FA
��A	�A�AA�FA �A|�Al�A;dAv�A �@�-@���@�M�@�%@��
@��@�X@�o@�7L@�9@�Q�@�K�@�@�G�@�
=@��@�@���@��#@�9X@���@�p�@�?}@�/@�j@�;d@އ+@�E�@�{@�`B@��`@���@�Ĝ@ܣ�@�z�@��;@ە�@�o@�=q@��@��`@��
@�C�@�=q@��T@ղ-@Չ7@�p�@�?}@�  @�=q@Ѻ^@�X@�&�@���@� �@ϥ�@ΰ!@�-@���@́@�?}@�1'@�~�@���@�7L@ȓu@�  @�+@��@�M�@Ł@��@���@ě�@�1@å�@ÍP@�33@���@�-@���@�?}@��@��
@��@���@��@��@���@���@�n�@�J@��`@�A�@��
@�S�@�
=@��@��@�ȴ@���@�M�@�/@��@�%@��j@��D@�r�@�9X@��m@�K�@��H@�
=@�"�@��H@�n�@���@���@���@���@�Ĝ@�bN@��@�l�@�o@�E�@��@��@�hs@�V@���@�r�@�  @��@�;d@�dZ@��@�{@��T@��T@��h@��@���@��H@�ȴ@���@��\@��\@��+@��\@���@�ff@�$�@��-@�&�@��@�A�@��@��w@��@�;d@��@��R@��+@�ff@�-@�@���@��h@��7@�x�@�x�@�?}@���@�Q�@�9X@��;@��@�"�@��@��R@��+@�^5@�J@�7L@���@��@�1@���@�dZ@�"�@��\@�=q@�@��h@���@���@�z�@�I�@�1'@��@�ƨ@�t�@�"�@���@�-@��T@���@��-@��7@�hs@�/@��@�Ĝ@���@�1'@��w@���@���@��P@�t�@�\)@�S�@�;d@��@�@��y@���@�$�@���@���@��@�x�@�hs@�`B@�`B@�X@�&�@���@��`@��`@��/@���@���@� �@�ƨ@��P@�l�@�C�@�33@�"�@�o@��H@��+@�@��h@�?}@�%@���@�Ĝ@�(�@�1@���@��@���@~kQ@l��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BYBZBYBYBYBYBYBZBZB\)Bm�B��B�sB	�B
K�B
L�B
I�B
E�B
B�B
6FB
.B
)�B
+B
,B
-B
/B
1'B
B�B
R�B
hsB
hsB
gmB
n�B
�%B
��B
��B
�oB
��B
��B
��B
��B
��B
�B
�jB
�/BBB
��B
��B  B
=B�B�B�B�B!�B%�B(�B?}B@�BR�B��B�FB��BBB1B\B{B{B
=B{B�B;dBH�BM�BF�BH�BR�BJ�BB�B9XB8RB1'B)�B�B�B{BbBVB
=B��B�B�fB��B�RB��B� Bo�B_;BJ�B/B �B\B
��B
�sB
�B
��B
�B
��B
�B
dZB
O�B
B�B
=qB
6FB
1'B
(�B
�B
PB	�/B	��B	�'B	�oB	�DB	�B	|�B	aHB	O�B	9XB	�B	PB	B��B�ZB�B��BƨB��B�3B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�!B�-B�FB�LB�FB�LB�9B�9B�FB�FB�FB�?B�9B�FB�^B�wB��BBĜBŢBȴB��B��B��B�
B�B�B�B�#B�/B�ZB�fB�sB�sB�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B	  B	B	B	+B	
=B	JB	VB	bB	oB	uB	{B	{B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	$�B	'�B	(�B	(�B	.B	9XB	<jB	?}B	A�B	E�B	K�B	N�B	T�B	XB	[#B	^5B	`BB	cTB	dZB	e`B	ffB	ffB	hsB	iyB	iyB	l�B	n�B	o�B	o�B	q�B	s�B	s�B	s�B	x�B	z�B	y�B	|�B	� B	�B	�B	�B	�B	�%B	�+B	�1B	�JB	�\B	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�9B	�9B	�FB	�LB	�LB	�RB	�XB	�^B	�^B	�dB	�jB	�qB	��B	ÖB	ƨB	ƨB	ǮB	ƨB	ǮB	ƨB	ŢB	ŢB	ŢB	ŢB	ŢB	ŢB	ŢB	ŢB	ĜB	ĜB	ŢB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�#B	�)B	�;B	�;B	�;B	�5B	�5B	�BB	�HB	�HB	�NB	�NB	�NB	�TB	�TB	�ZB	�`B	�`B	�fB	�fB	�fB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
%�B
/ 22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  BYBZBYBYBYBYBYBZBZB\)Bm�B��B�sB	�B
K�B
L�B
I�B
E�B
B�B
6FB
.B
)�B
+B
,B
-B
/B
1'B
B�B
R�B
hsB
hsB
gmB
n�B
�%B
��B
��B
�oB
��B
��B
��B
��B
��B
�B
�jB
�/BBB
��B
��B  B
=B�B�B�B�B!�B%�B(�B?}B@�BR�B��B�FB��BBB1B\B{B{B
=B{B�B;dBH�BM�BF�BH�BR�BJ�BB�B9XB8RB1'B)�B�B�B{BbBVB
=B��B�B�fB��B�RB��B� Bo�B_;BJ�B/B �B\B
��B
�sB
�B
��B
�B
��B
�B
dZB
O�B
B�B
=qB
6FB
1'B
(�B
�B
PB	�/B	��B	�'B	�oB	�DB	�B	|�B	aHB	O�B	9XB	�B	PB	B��B�ZB�B��BƨB��B�3B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�!B�-B�FB�LB�FB�LB�9B�9B�FB�FB�FB�?B�9B�FB�^B�wB��BBĜBŢBȴB��B��B��B�
B�B�B�B�#B�/B�ZB�fB�sB�sB�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B	  B	B	B	+B	
=B	JB	VB	bB	oB	uB	{B	{B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	$�B	'�B	(�B	(�B	.B	9XB	<jB	?}B	A�B	E�B	K�B	N�B	T�B	XB	[#B	^5B	`BB	cTB	dZB	e`B	ffB	ffB	hsB	iyB	iyB	l�B	n�B	o�B	o�B	q�B	s�B	s�B	s�B	x�B	z�B	y�B	|�B	� B	�B	�B	�B	�B	�%B	�+B	�1B	�JB	�\B	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�9B	�9B	�FB	�LB	�LB	�RB	�XB	�^B	�^B	�dB	�jB	�qB	��B	ÖB	ƨB	ƨB	ǮB	ƨB	ǮB	ƨB	ŢB	ŢB	ŢB	ŢB	ŢB	ŢB	ŢB	ŢB	ĜB	ĜB	ŢB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�#B	�)B	�;B	�;B	�;B	�5B	�5B	�BB	�HB	�HB	�NB	�NB	�NB	�TB	�TB	�ZB	�`B	�`B	�fB	�fB	�fB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
%�B
/ 22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.13 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190548                              AO  ARCAADJP                                                                    20181005190548    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190548  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190548  QCF$                G�O�G�O�G�O�8000            