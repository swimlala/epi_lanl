CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:31Z creation      
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
_FillValue                 �  A8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \x   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^l   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �`   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �`   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �`   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �`   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005190531  20181005190531  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               vA   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��'k�271   @��'�O��@1KC��%�c�9XbN1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      vA   A   A   @�  @���A   A   A>ffA`  A~ffA�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0ffB8  B@ffBHffBPffBXffB`ffBh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�33BЙ�Bә�B�  B�  B�  B�  B�  B�  B���B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C%�fC(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL�CN�CP  CR  CT�CV  CX  CZ  C\  C^  C`  Cb  Cd  Ce�fCh  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz�C|  C~  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C��3C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C��3C��3C��3C��3C��3C�  C�  C�  C��3C��3C�  D   D � D  D� D  D�fD  D� DfD� D  D� DfD�fD  D� DfD�fD	  D	� D
  D
� D  D� DfD� D��Dy�D��D� D  Dy�D  D� D  D� DfD� D��D�fD  D� D  D� DfD� DfD�fD  D� D��D� D  D� DfD� D��D� D  D� D��D� D  D� D fD � D ��D!� D"  D"� D#  D#� D$  D$y�D$��D%� D&  D&� D'  D'� D(  D(y�D(��D)y�D*  D*� D+  D+� D,  D,� D-  D-�fD.fD.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3�fD4  D4� D4��D5y�D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D<��D=y�D=��D>� D?fD?� D@  D@� DA  DA� DB  DB� DCfDC�fDD  DD� DE  DE�fDF  DFy�DG  DG� DH  DH�fDI  DI� DJ  DJ� DK  DKy�DL  DL� DL��DM� DN  DN� DO  DO� DO��DP�fDQfDQ� DQ��DR� DS  DS� DT  DTy�DU  DU�fDV  DV� DW  DW� DX  DX� DYfDY�fDZ  DZ� D[  D[� D[��D\� D]fD]� D^  D^� D^��D_� D`fD`� D`��Day�Db  Db�fDc  Dcy�Dd  Dd�fDefDe� Df  Dfy�Df��Dg� Dh  Dh� Dh��Di� Dj  Dj� DkfDk� Dl  Dl� Dm  Dmy�Dm��Dn� DofDo�fDpfDp�fDq  Dq� Dr  Dry�Ds  Ds� Dt  Dt� Du  Du�fDv  Dv� Dw  Dw� Dw�fDy��D�8�D�Ӆ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�(�@���A{A"{A@z�Ab{A�=pA�
=A�
=A�
=A�
=A�
=A�
=A�
=B �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�u�B�B�B�B�B�u�B��)B��)B�B�B�B�B�B�B�B�B�B�B�B�B�\B�B�B�B�B�B�C !HC!HC!HC!HC!HC
!HC!HC!HC!HC!HC!HC!HC!HC!HC!HC!HC !HC"!HC$!HC&�C(!HC*!HC,!HC.!HC0!HC2!HC4!HC6!HC8!HC:!HC<!HC>!HC@!HCB!HCD!HCF!HCH!HCJ!HCL:�CN:�CP!HCR!HCT:�CV!HCX!HCZ!HC\!HC^!HC`!HCb!HCd!HCf�Ch!HCj!HCl!HCn!HCp!HCr!HCt!HCv!HCx!HCz:�C|!HC~!HC��C��C��C�qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�qC��C��C��C��C��C��C��C��C��C�qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C�qC��C�qC��C��C��C��C��C��C��C�qC��C��C��C��C��C��C��C��C��C��C��C��C�qC��C��C��C��C��C��C��C��C��C�qC�qC��C��C��C��C��C��C�qC�qC�qC��C��C��C��C��C��C��C��C��C��C��C��C��D RD �RDRD�RDRD��DRD�RD�D�RDRD�RD�D��DRD�RD�D��D	RD	�RD
RD
�RDRD�RD�D�RD�D��D�D�RDRD��DRD�RDRD�RD�D�RD�D��DRD�RDRD�RD�D�RD�D��DRD�RD�D�RDRD�RD�D�RD�D�RDRD�RD�D�RDRD�RD �D �RD!�D!�RD"RD"�RD#RD#�RD$RD$��D%�D%�RD&RD&�RD'RD'�RD(RD(��D)�D)��D*RD*�RD+RD+�RD,RD,�RD-RD-��D.�D.�RD/RD/�RD0RD0�RD1RD1�RD2RD2�RD3RD3��D4RD4�RD5�D5��D6RD6�RD7RD7�RD8RD8�RD9RD9�RD:RD:�RD;RD;�RD<RD<�RD=�D=��D>�D>�RD?�D?�RD@RD@�RDARDA�RDBRDB�RDC�DC��DDRDD�RDERDE��DFRDF��DGRDG�RDHRDH��DIRDI�RDJRDJ�RDKRDK��DLRDL�RDM�DM�RDNRDN�RDORDO�RDP�DP��DQ�DQ�RDR�DR�RDSRDS�RDTRDT��DURDU��DVRDV�RDWRDW�RDXRDX�RDY�DY��DZRDZ�RD[RD[�RD\�D\�RD]�D]�RD^RD^�RD_�D_�RD`�D`�RDa�Da��DbRDb��DcRDc��DdRDd��De�De�RDfRDf��Dg�Dg�RDhRDh�RDi�Di�RDjRDj�RDk�Dk�RDlRDl�RDmRDm��Dn�Dn�RDo�Do��Dp�Dp��DqRDq�RDrRDr��DsRDs�RDtRDt�RDuRDu��DvRDv�RDwRDw�RDw�Dy�HD�=D�׮111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��HA��;A��;A��HA��;A��;A��TA��mA��`A��mA��A��A��A��A��A��A��A��A��A���A���A���A���A���A���A�  A�  A�  A�  A�  A�  A�  A�A�A�%A�1A�
=A�1A�%A�%A�%A�1A�
=A�VA�%A�%A�A�%A��mA���A�x�A�n�A�bNA�z�A�
=A�ƨAə�A�5?Aȏ\A��`A�K�A���A�\)A��A�VA�\)A�XA�^5A��A�"�A�C�A�A��mA��RA�%A�dZA�p�A�%A��#A��+A�x�A�n�A��A���A���A� �A��DA�
=A�hsA�
=A��A�Q�A���A�I�A��HA��A��A��mA��7A�E�A��!A���A�E�A�  A� �A���A��+A��PA�A��yA�;dA��A�t�A~9XAx�+Aw%At=qAp�jAn��Amt�Ajz�Ai��Ail�AiO�AiK�Ai7LAhA�Ab��A_x�A]p�A\AX�ATQ�AS��AShsAS�ARAP�AN�\AM%ALJAHv�AD�AAhsA>5?A<�A;7LA9&�A8  A5?}A2�+A0�A0^5A.�A-��A-"�A+�A*�A*~�A)�^A(��A'ƨA'�FA'�hA&jA$-A#/A"��A!x�A ��A��A�AAhsA��A�7AE�A�A|�AE�Ax�A��A�A
=A�
A��A`BA33A��A  Al�A
�/A
��A
ffA
�A	�A	��A	A�AO�AA�Ap�A�\A�mA�
A��A"�A ��@��+@�hs@���@���@�&�@���@�l�@���@�b@���@��@�=q@��@�F@��H@��@�ƨ@�ff@���@�^@��@�7L@�r�@�{@�V@�P@��@�!@�  @�h@䛦@��@�$�@�&�@��H@�C�@ؓu@��
@�t�@��@��@�+@�^5@�J@��@��@�j@�b@��@�dZ@�l�@�t�@�;d@�o@Ώ\@���@�(�@�K�@�E�@��@�I�@��@��;@���@ǝ�@�o@Ɵ�@�ff@�^5@�5?@��@���@Ł@�G�@���@�z�@�Z@�A�@��@å�@�M�@���@��@��^@�?}@��@�r�@�t�@�ȴ@�n�@�$�@�@���@��-@�X@�X@�&�@�bN@�bN@��@��@�+@�@�K�@��@���@��y@�@��-@��@���@��9@�z�@�b@��@���@�
=@���@�V@��#@�O�@���@��@��m@���@�S�@���@�-@��@��/@�bN@��F@�S�@�"�@��T@��@��@�bN@�1'@�1@�K�@��@�`B@���@���@��@�bN@�Q�@�Q�@��m@��F@�"�@���@�5?@�{@�{@���@�O�@�/@���@��@��@��@��@�\)@�ȴ@��+@���@���@���@�n�@�$�@�hs@�7L@�V@�Ĝ@�bN@�9X@��
@�l�@�@���@�-@��@��T@���@�&�@��j@�Z@��@�  @��@��@�S�@�
=@��!@�~�@�M�@�-@��@�J@���@��^@��7@��@�O�@�O�@�7L@���@�I�@��
@��
@��@��@���@���@��@�v�@�M�@��@��@��-@��7@�hs@�`B@�V@��/@���@��9@���@��u@�z�@�A�@�1@���@�\)@�C�@��@��\@�ff@��@���@��h@�X@��j@��@�r�@�r�@�r�@�bN@���@��@�K�@��H@���@���@���@���@��@�ȴ@��+@�n�@�ff@�^5@�J@���@��#@��7@�?}@��@��@�Z@�b@��@��
@���@��@�|�@�t�@�33@���@�ȴ@��+@���@�O�@�&�@���@�Z@{��@j}V111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��HA��;A��;A��HA��;A��;A��TA��mA��`A��mA��A��A��A��A��A��A��A��A��A���A���A���A���A���A���A�  A�  A�  A�  A�  A�  A�  A�A�A�%A�1A�
=A�1A�%A�%A�%A�1A�
=A�VA�%A�%A�A�%A��mA���A�x�A�n�A�bNA�z�A�
=A�ƨAə�A�5?Aȏ\A��`A�K�A���A�\)A��A�VA�\)A�XA�^5A��A�"�A�C�A�A��mA��RA�%A�dZA�p�A�%A��#A��+A�x�A�n�A��A���A���A� �A��DA�
=A�hsA�
=A��A�Q�A���A�I�A��HA��A��A��mA��7A�E�A��!A���A�E�A�  A� �A���A��+A��PA�A��yA�;dA��A�t�A~9XAx�+Aw%At=qAp�jAn��Amt�Ajz�Ai��Ail�AiO�AiK�Ai7LAhA�Ab��A_x�A]p�A\AX�ATQ�AS��AShsAS�ARAP�AN�\AM%ALJAHv�AD�AAhsA>5?A<�A;7LA9&�A8  A5?}A2�+A0�A0^5A.�A-��A-"�A+�A*�A*~�A)�^A(��A'ƨA'�FA'�hA&jA$-A#/A"��A!x�A ��A��A�AAhsA��A�7AE�A�A|�AE�Ax�A��A�A
=A�
A��A`BA33A��A  Al�A
�/A
��A
ffA
�A	�A	��A	A�AO�AA�Ap�A�\A�mA�
A��A"�A ��@��+@�hs@���@���@�&�@���@�l�@���@�b@���@��@�=q@��@�F@��H@��@�ƨ@�ff@���@�^@��@�7L@�r�@�{@�V@�P@��@�!@�  @�h@䛦@��@�$�@�&�@��H@�C�@ؓu@��
@�t�@��@��@�+@�^5@�J@��@��@�j@�b@��@�dZ@�l�@�t�@�;d@�o@Ώ\@���@�(�@�K�@�E�@��@�I�@��@��;@���@ǝ�@�o@Ɵ�@�ff@�^5@�5?@��@���@Ł@�G�@���@�z�@�Z@�A�@��@å�@�M�@���@��@��^@�?}@��@�r�@�t�@�ȴ@�n�@�$�@�@���@��-@�X@�X@�&�@�bN@�bN@��@��@�+@�@�K�@��@���@��y@�@��-@��@���@��9@�z�@�b@��@���@�
=@���@�V@��#@�O�@���@��@��m@���@�S�@���@�-@��@��/@�bN@��F@�S�@�"�@��T@��@��@�bN@�1'@�1@�K�@��@�`B@���@���@��@�bN@�Q�@�Q�@��m@��F@�"�@���@�5?@�{@�{@���@�O�@�/@���@��@��@��@��@�\)@�ȴ@��+@���@���@���@�n�@�$�@�hs@�7L@�V@�Ĝ@�bN@�9X@��
@�l�@�@���@�-@��@��T@���@�&�@��j@�Z@��@�  @��@��@�S�@�
=@��!@�~�@�M�@�-@��@�J@���@��^@��7@��@�O�@�O�@�7L@���@�I�@��
@��
@��@��@���@���@��@�v�@�M�@��@��@��-@��7@�hs@�`B@�V@��/@���@��9@���@��u@�z�@�A�@�1@���@�\)@�C�@��@��\@�ff@��@���@��h@�X@��j@��@�r�@�r�@�r�@�bN@���@��@�K�@��H@���@���@���@���@��@�ȴ@��+@�n�@�ff@�^5@�J@���@��#@��7@�?}@��@��@�Z@�b@��@��
@���@��@�|�@�t�@�33@���@�ȴ@��+@���@�O�@�&�@���@�Z@{��@j}V111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B1'B1'B2-B1'B2-B2-B1'B1'B2-B2-B1'B1'B1'B1'B2-B2-B1'B2-B2-B1'B2-B1'B1'B1'B1'B2-B2-B2-B2-B2-B2-B2-B2-B2-B2-B2-B2-B2-B2-B2-B1'B2-B2-B2-B1'B1'B2-B2-B49B5?B;dBffB|�B�oB��B�B�B�?B��B�TB��BB1BPBuB/B>wBR�B`BBaHBbNBgmBm�Bu�B~�B�B�B}�Bz�Bq�B\)B+B�B�B�B�BhB+B��B��B�B�/B�B��B��B��B�hB�7B�B{�Bm�BYBC�B5?B1'B+B\B
�
B
�%B
&�B
B	�5B	ĜB	��B	�B	t�B	cTB	M�B	;dB	0!B	#�B	�B	�B	�B	�B	�B	�B��B�HB��BȴB�XB��B��B��B��B��B�uB�\B�DB�B}�Bv�Bw�Bu�Bt�Bv�B|�B~�B�=B��B��B��B��B��B��B�B�3B�9B�FB�XB�^B�dB�XB�}BÖB�qB�dB�RB�9B�B��B��B��B��B�oB�oB�oB�uB��B��B��B��B��B��B��B��B��B��B��B�B�!B�'B�'B�'B�!B�B�'B�'B�'B�B�B��B�B�B�B�B�B�B�!B�-B�-B�9B�3B�FB�dB�}B�wB��BBƨB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�)B�mB��B��B�B�B�B�fB�)B�
B�B��B�B��B�B�ZB�fB�B�B�B��B��B	B	B	
=B	VB	\B	bB	{B	�B	�B	�B	�B	�B	�B	!�B	"�B	#�B	'�B	,B	/B	0!B	1'B	49B	5?B	8RB	9XB	:^B	;dB	?}B	A�B	B�B	C�B	G�B	K�B	P�B	T�B	[#B	[#B	]/B	\)B	[#B	[#B	[#B	\)B	]/B	bNB	dZB	gmB	hsB	hsB	iyB	iyB	jB	m�B	n�B	q�B	s�B	v�B	� B	�B	�B	�B	�+B	�+B	�1B	�7B	�=B	�=B	�JB	�PB	�\B	�bB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�3B	�FB	�RB	�RB	�LB	�LB	�LB	�RB	�XB	�dB	�jB	�qB	�jB	�jB	�jB	�jB	�wB	�wB	�}B	��B	ŢB	ƨB	ƨB	ƨB	ŢB	ƨB	ǮB	ƨB	ƨB	ƨB	ƨB	ǮB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�B	�B	�#B	�#B	�)B	�)B	�/B	�/B	�;B	�BB	�HB	�HB	�NB	�NB	�ZB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
+B
+B
+B
+B
+B
+B
+B
+B
+B
+B
%B
1B

=B

=B
DB

�B
yB
*�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B1'B1'B2-B1'B2-B2-B1'B1'B2-B2-B1'B1'B1'B1'B2-B2-B1'B2-B2-B1'B2-B1'B1'B1'B1'B2-B2-B2-B2-B2-B2-B2-B2-B2-B2-B2-B2-B2-B2-B2-B1'B2-B2-B2-B1'B1'B2-B2-B49B5?B;dBffB|�B�oB��B�B�B�?B��B�TB��BB1BPBuB/B>wBR�B`BBaHBbNBgmBm�Bu�B~�B�B�B}�Bz�Bq�B\)B+B�B�B�B�BhB+B��B��B�B�/B�B��B��B��B�hB�7B�B{�Bm�BYBC�B5?B1'B+B\B
�
B
�%B
&�B
B	�5B	ĜB	��B	�B	t�B	cTB	M�B	;dB	0!B	#�B	�B	�B	�B	�B	�B	�B��B�HB��BȴB�XB��B��B��B��B��B�uB�\B�DB�B}�Bv�Bw�Bu�Bt�Bv�B|�B~�B�=B��B��B��B��B��B��B�B�3B�9B�FB�XB�^B�dB�XB�}BÖB�qB�dB�RB�9B�B��B��B��B��B�oB�oB�oB�uB��B��B��B��B��B��B��B��B��B��B��B�B�!B�'B�'B�'B�!B�B�'B�'B�'B�B�B��B�B�B�B�B�B�B�!B�-B�-B�9B�3B�FB�dB�}B�wB��BBƨB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�)B�mB��B��B�B�B�B�fB�)B�
B�B��B�B��B�B�ZB�fB�B�B�B��B��B	B	B	
=B	VB	\B	bB	{B	�B	�B	�B	�B	�B	�B	!�B	"�B	#�B	'�B	,B	/B	0!B	1'B	49B	5?B	8RB	9XB	:^B	;dB	?}B	A�B	B�B	C�B	G�B	K�B	P�B	T�B	[#B	[#B	]/B	\)B	[#B	[#B	[#B	\)B	]/B	bNB	dZB	gmB	hsB	hsB	iyB	iyB	jB	m�B	n�B	q�B	s�B	v�B	� B	�B	�B	�B	�+B	�+B	�1B	�7B	�=B	�=B	�JB	�PB	�\B	�bB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�3B	�FB	�RB	�RB	�LB	�LB	�LB	�RB	�XB	�dB	�jB	�qB	�jB	�jB	�jB	�jB	�wB	�wB	�}B	��B	ŢB	ƨB	ƨB	ƨB	ŢB	ƨB	ǮB	ƨB	ƨB	ƨB	ƨB	ǮB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�B	�B	�#B	�#B	�)B	�)B	�/B	�/B	�;B	�BB	�HB	�HB	�NB	�NB	�ZB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
+B
+B
+B
+B
+B
+B
+B
+B
+B
+B
%B
1B

=B

=B
DB

�B
yB
*�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.13 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190531                              AO  ARCAADJP                                                                    20181005190531    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190531  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190531  QCF$                G�O�G�O�G�O�8000            