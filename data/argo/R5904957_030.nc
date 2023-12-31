CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:09Z creation      
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
_FillValue                 �  A4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \h   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^\   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �H   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �h   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �l   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �p   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �t   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �x   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181024140809  20181024140809  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @׷$���1   @׷%@yoL@3DZ�1�c�$�/�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   A   @�33@�  A   A   A@  Aa��A�  A�  A�  A�  A�  A�  A�  A���B   B  B  B  B   B(  B0  B8  B?��BG��BP  BX  B`  Bh  BpffBx  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C�fC�fC  C  C  C  C   C"  C$  C%�fC(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@�CA�fCD  CF  CH  CI�fCL  CN  CP  CR  CT  CV  CX  CZ  C\  C^�C`  Cb  Cc�fCf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cy�fC|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C�  C��D fD � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D�fDfD� D  D� D  D� D  D� DfD�fD  D� D  D� D  Dy�D��D� D  D� D  D� D  D�fD  D� D  D� DfD� D  D� D  D� D  D� DfD�fD   D � D!  D!� D"  D"� D#  D#y�D$  D$� D%  D%� D&  D&� D'  D'� D(fD(�fD)  D)� D*  D*� D+  D+� D+��D,� D-fD-� D.  D.� D/  D/y�D0  D0� D0��D1y�D2  D2� D3  D3�fD4  D4� D5fD5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� D@��DA� DB  DB� DC  DC�fDD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DIy�DJ  DJ� DK  DK� DL  DL� DM  DMy�DM��DN� DO  DO� DP  DP� DP��DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DX��DY� DZ  DZ� D[fD[� D\  D\� D\��D]� D^  D^� D_  D_� D`  D`� DafDa� Db  Db�fDcfDc� Dd  Ddy�De  De� Df  Df� Dg  Dg�fDh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do�fDp  Dpy�Dp��Dq� Dr  Dr�fDs  Ds�fDtfDt�fDu  Duy�Dv  Dv�fDwfDws3Dyg�D�&fD�nf11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�\)@�(�A{A"{AB{Ac�A�
=A�
=A�
=A�
=A�
=A�
=A�
=A��
B �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�B�B�B�B�B�B�\B�\B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�u�B�u�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C !HC!HC!HC!HC!HC
!HC!HC!HC!HC!HC�C�C!HC!HC!HC!HC !HC"!HC$!HC&�C(!HC*!HC,!HC.!HC0!HC2!HC4!HC6!HC8!HC:!HC<!HC>!HC@:�CB�CD!HCF!HCH!HCJ�CL!HCN!HCP!HCR!HCT!HCV!HCX!HCZ!HC\!HC^:�C`!HCb!HCd�Cf!HCh!HCj!HCl!HCn!HCp!HCr!HCt!HCv!HCx!HCz�C|!HC~!HC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�qC��C��C��C��C��C��C��C�qC�qC�qC��C��C��C��C��C�qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�qC��C��C��C��C��C��C�qD �D �RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD	RD	�RD
RD
�RDRD�RDRD��D�D�RDRD�RDRD�RDRD�RD�D��DRD�RDRD�RDRD��D�D�RDRD�RDRD�RDRD��DRD�RDRD�RD�D�RDRD�RDRD�RDRD�RD�D��D RD �RD!RD!�RD"RD"�RD#RD#��D$RD$�RD%RD%�RD&RD&�RD'RD'�RD(�D(��D)RD)�RD*RD*�RD+RD+�RD,�D,�RD-�D-�RD.RD.�RD/RD/��D0RD0�RD1�D1��D2RD2�RD3RD3��D4RD4�RD5�D5�RD6RD6�RD7RD7�RD8RD8�RD9RD9�RD:RD:�RD;RD;�RD<RD<�RD=RD=�RD>RD>�RD?RD?�RD@RD@�RDA�DA�RDBRDB�RDCRDC��DDRDD�RDERDE�RDFRDF�RDGRDG�RDHRDH�RDIRDI��DJRDJ�RDKRDK�RDLRDL�RDMRDM��DN�DN�RDORDO�RDPRDP�RDQ�DQ�RDRRDR�RDSRDS�RDTRDT�RDURDU�RDVRDV�RDWRDW�RDXRDX�RDY�DY�RDZRDZ�RD[�D[�RD\RD\�RD]�D]�RD^RD^�RD_RD_�RD`RD`�RDa�Da�RDbRDb��Dc�Dc�RDdRDd��DeRDe�RDfRDf�RDgRDg��DhRDh�RDiRDi�RDjRDj�RDkRDk�RDlRDl�RDmRDm�RDnRDn�RDoRDo��DpRDp��Dq�Dq�RDrRDr��DsRDs��Dt�Dt��DuRDu��DvRDv��Dw�Dw{�Dyp D�*�D�r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��;A�  A��A��A��A��A��A��A��A��A��A� �A��A��A��A��A��A��A��A��A�
=A��
A�O�A�p�A�;dA��HAғuA�33A�;dA��A�~�A�M�A���AЙ�A�1'A���A�|�A�JAκ^A�t�A�bA�&�A�I�A�/A���A��yA�x�A�9XA��/A�(�A���A�ȴA�\)A�bNA�S�A�5?A���A��A�JA�Q�A��9A��A�|�A�x�A�{A��9A�bNA�XA�A��jA�A���A���A��A���A��+A��A�M�A�VA��;A���A��HA�|�A��A��RA���A�oA�x�A�VA�I�A�1'A���A�"�A��A�n�A���A�VA���A�"�A�VA��`A�7LA�$�A�O�A�VA�C�A���A�bA��A��A���A���A�l�A�l�A�ƨA�5?A��DAwdZAr�!Ap��Ao�PAn��Al�!Alr�Ak��Ag�hAfAel�Ad1'Ab�DA`�A[AVVAT��AT(�AQ�AQO�AQoAO`BAL��AJ��AJ(�AJ�AJAI��AIhsAH��AHE�AFĜAD�AC�ACp�AB�/A?��A<�\A:~�A8��A7��A7x�A7K�A6 �A4��A4��A4�A3p�A2�DA1�7A/�A. �A,�+A+�-A+t�A*�yA(��A(�A'�TA'"�A&�9A&9XA%\)A$JA#/A"��A!�TA�mA7LA��Az�Ap�A�AZAbNA�-AA�
A�A�A��A�A�AA�\AbA��AZA1'AAA��A&�A
�A
�A��A�FA`BAbNA��A�7A��A��Av�A�PAp�A�A Z@���@�O�@�z�@��@�@�`B@��F@�~�@�x�@�V@�S�@�ȴ@�~�@�G�@�@�z�@�Z@��y@�5?@�$�@�@��@�dZ@���@�b@��@��@�ȴ@���@�o@�@�7L@��/@ӕ�@҇+@�{@�x�@���@��y@·+@Ώ\@·+@��#@���@̋D@�9X@�A�@̼j@��/@�Ĝ@�1'@˶F@�33@���@�@�x�@�%@ț�@��@�C�@Ɵ�@�E�@Ł@ēu@��@öF@��@�^5@��#@�X@��
@���@�ff@�E�@�J@���@�G�@��D@���@�;d@�@��y@�ȴ@��+@���@�O�@��;@�@�p�@��`@���@���@��@��-@���@��H@���@��u@�Z@�9X@���@�33@���@�5?@��@��#@�G�@�Ĝ@�9X@�\)@��@�{@�hs@��`@�Ĝ@���@��@�Z@��@�\)@��@���@��H@��R@�n�@��@��^@�x�@�7L@�/@�7L@�/@�/@���@���@� �@�  @��
@��P@�\)@��P@�"�@���@��H@�"�@��!@�o@��H@��@�+@�"�@�o@��R@�@�x�@�V@���@���@�z�@��@�t�@�S�@��y@��\@�~�@�v�@�=q@�J@���@��@���@�?}@��@�Z@�1'@�(�@��;@�|�@�+@��@�@���@���@��R@���@�ff@�5?@��@�@�O�@��@���@�Q�@�  @��w@�+@��H@���@�v�@�M�@��@��-@��@�Ĝ@���@��D@�r�@�A�@�
=@�ȴ@��!@�v�@�{@���@��h@��/@�Q�@�9X@�b@��m@���@�ƨ@��@�dZ@�K�@��@���@�n�@�-@���@��#@�?}@�V@��/@��9@��@��@��@�+@��!@�~�@�^5@�-@�@��@��@��@��#@���@��^@���@�x�@�O�@��@���@���@�r�@�Q�@�(�@���@��;@��
@���@���@�ƨ@��w@��F@��@���@�5�@st�@b�L11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��;A�  A��A��A��A��A��A��A��A��A��A� �A��A��A��A��A��A��A��A��A�
=A��
A�O�A�p�A�;dA��HAғuA�33A�;dA��A�~�A�M�A���AЙ�A�1'A���A�|�A�JAκ^A�t�A�bA�&�A�I�A�/A���A��yA�x�A�9XA��/A�(�A���A�ȴA�\)A�bNA�S�A�5?A���A��A�JA�Q�A��9A��A�|�A�x�A�{A��9A�bNA�XA�A��jA�A���A���A��A���A��+A��A�M�A�VA��;A���A��HA�|�A��A��RA���A�oA�x�A�VA�I�A�1'A���A�"�A��A�n�A���A�VA���A�"�A�VA��`A�7LA�$�A�O�A�VA�C�A���A�bA��A��A���A���A�l�A�l�A�ƨA�5?A��DAwdZAr�!Ap��Ao�PAn��Al�!Alr�Ak��Ag�hAfAel�Ad1'Ab�DA`�A[AVVAT��AT(�AQ�AQO�AQoAO`BAL��AJ��AJ(�AJ�AJAI��AIhsAH��AHE�AFĜAD�AC�ACp�AB�/A?��A<�\A:~�A8��A7��A7x�A7K�A6 �A4��A4��A4�A3p�A2�DA1�7A/�A. �A,�+A+�-A+t�A*�yA(��A(�A'�TA'"�A&�9A&9XA%\)A$JA#/A"��A!�TA�mA7LA��Az�Ap�A�AZAbNA�-AA�
A�A�A��A�A�AA�\AbA��AZA1'AAA��A&�A
�A
�A��A�FA`BAbNA��A�7A��A��Av�A�PAp�A�A Z@���@�O�@�z�@��@�@�`B@��F@�~�@�x�@�V@�S�@�ȴ@�~�@�G�@�@�z�@�Z@��y@�5?@�$�@�@��@�dZ@���@�b@��@��@�ȴ@���@�o@�@�7L@��/@ӕ�@҇+@�{@�x�@���@��y@·+@Ώ\@·+@��#@���@̋D@�9X@�A�@̼j@��/@�Ĝ@�1'@˶F@�33@���@�@�x�@�%@ț�@��@�C�@Ɵ�@�E�@Ł@ēu@��@öF@��@�^5@��#@�X@��
@���@�ff@�E�@�J@���@�G�@��D@���@�;d@�@��y@�ȴ@��+@���@�O�@��;@�@�p�@��`@���@���@��@��-@���@��H@���@��u@�Z@�9X@���@�33@���@�5?@��@��#@�G�@�Ĝ@�9X@�\)@��@�{@�hs@��`@�Ĝ@���@��@�Z@��@�\)@��@���@��H@��R@�n�@��@��^@�x�@�7L@�/@�7L@�/@�/@���@���@� �@�  @��
@��P@�\)@��P@�"�@���@��H@�"�@��!@�o@��H@��@�+@�"�@�o@��R@�@�x�@�V@���@���@�z�@��@�t�@�S�@��y@��\@�~�@�v�@�=q@�J@���@��@���@�?}@��@�Z@�1'@�(�@��;@�|�@�+@��@�@���@���@��R@���@�ff@�5?@��@�@�O�@��@���@�Q�@�  @��w@�+@��H@���@�v�@�M�@��@��-@��@�Ĝ@���@��D@�r�@�A�@�
=@�ȴ@��!@�v�@�{@���@��h@��/@�Q�@�9X@�b@��m@���@�ƨ@��@�dZ@�K�@��@���@�n�@�-@���@��#@�?}@�V@��/@��9@��@��@��@�+@��!@�~�@�^5@�-@�@��@��@��@��#@���@��^@���@�x�@�O�@��@���@���@�r�@�Q�@�(�@���@��;@��
@���@���@�ƨ@��w@��F@��@���@�5�@st�@b�L11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�JB
�\B
�hB
�oB
�oB
�oB
�oB
�oB
�oB
�oB
�oB
�oB
�uB
�{B
�{B
�uB
��B
��B
��B
��B
��B
��B
�9B
�sB
��B
��B
��B
��BDB�B�B�B"�B.B6FB9XB@�BP�B[#BaHBv�Bq�Bl�Bw�B�B�\B��B�jBȴB��BÖB��B�B�BbB$�B,B.BN�Bv�B��B��B��B�dBĜB�3B�9B�?B�3B�-B�jB��B�FB�B��B��B��B��B��B��B��B��B��B��B�uB�\B�Bz�Bn�B]/BZBT�B�B��B�#B�-B�uB{�Bo�Bl�BgmBYB=qB�BDBDBDB
��B
�NB
��B
��B
�B
��B
�DB
�B
ffB
7LB	�B	��B	ĜB	�jB	�FB	�B	��B	��B	�7B	�B	{�B	s�B	gmB	YB	:^B	"�B	�B	�B	JB	+B	B��B��B�B�B�B�B�yB�fB�ZB�NB�HB�)B�B��B��B��B�^B�FB�-B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�bB�oB�uB�{B�{B�uB�uB�{B��B�uB�oB�oB�hB�\B�VB�PB�JB�DB�JB�PB�hB��B��B��B��B��B��B��B��B��B��B��B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�bB�hB�{B�{B�uB�hB�\B�oB�oB�hB�hB�{B�{B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�3BBƨBǮB��B��B��B�B�#B�)B�/B�BB�NB�TB�ZB�fB�yB�B�B�B�B��B��B��B��B��B	B	
=B	\B	hB	hB	oB	uB	�B	�B	�B	"�B	#�B	#�B	$�B	%�B	'�B	(�B	(�B	'�B	+B	.B	1'B	6FB	:^B	33B	-B	-B	1'B	7LB	8RB	9XB	;dB	@�B	E�B	K�B	R�B	VB	YB	]/B	`BB	`BB	`BB	cTB	hsB	l�B	l�B	m�B	m�B	n�B	o�B	t�B	v�B	w�B	x�B	y�B	{�B	� B	�B	�B	�B	�%B	�1B	�1B	�7B	�=B	�JB	�VB	�\B	�bB	�oB	��B	��B	��B	��B	��B	��B	�B	�?B	�^B	�wB	�}B	��B	��B	ÖB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�)B	�/B	�/B	�5B	�;B	�BB	�BB	�BB	�HB	�HB	�HB	�NB	�NB	�TB	�ZB	�ZB	�`B	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
%B
%B
+B
+B
1B
	7B
VB
\B
\B
\B
\B
\B
\B
\B
\B
\B
\B
\B
bB
bB
hB
hB
hB
oB
oB
oB
oB
oB
oB
oB
oB
oB
uB
�B
&�B
6F11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
�JB
�\B
�hB
�oB
�oB
�oB
�oB
�oB
�oB
�oB
�oB
�oB
�uB
�{B
�{B
�uB
��B
��B
��B
��B
��B
��B
�9B
�sB
��B
��B
��B
��BDB�B�B�B"�B.B6FB9XB@�BP�B[#BaHBv�Bq�Bl�Bw�B�B�\B��B�jBȴB��BÖB��B�B�BbB$�B,B.BN�Bv�B��B��B��B�dBĜB�3B�9B�?B�3B�-B�jB��B�FB�B��B��B��B��B��B��B��B��B��B��B�uB�\B�Bz�Bn�B]/BZBT�B�B��B�#B�-B�uB{�Bo�Bl�BgmBYB=qB�BDBDBDB
��B
�NB
��B
��B
�B
��B
�DB
�B
ffB
7LB	�B	��B	ĜB	�jB	�FB	�B	��B	��B	�7B	�B	{�B	s�B	gmB	YB	:^B	"�B	�B	�B	JB	+B	B��B��B�B�B�B�B�yB�fB�ZB�NB�HB�)B�B��B��B��B�^B�FB�-B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�bB�oB�uB�{B�{B�uB�uB�{B��B�uB�oB�oB�hB�\B�VB�PB�JB�DB�JB�PB�hB��B��B��B��B��B��B��B��B��B��B��B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�bB�hB�{B�{B�uB�hB�\B�oB�oB�hB�hB�{B�{B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�3BBƨBǮB��B��B��B�B�#B�)B�/B�BB�NB�TB�ZB�fB�yB�B�B�B�B��B��B��B��B��B	B	
=B	\B	hB	hB	oB	uB	�B	�B	�B	"�B	#�B	#�B	$�B	%�B	'�B	(�B	(�B	'�B	+B	.B	1'B	6FB	:^B	33B	-B	-B	1'B	7LB	8RB	9XB	;dB	@�B	E�B	K�B	R�B	VB	YB	]/B	`BB	`BB	`BB	cTB	hsB	l�B	l�B	m�B	m�B	n�B	o�B	t�B	v�B	w�B	x�B	y�B	{�B	� B	�B	�B	�B	�%B	�1B	�1B	�7B	�=B	�JB	�VB	�\B	�bB	�oB	��B	��B	��B	��B	��B	��B	�B	�?B	�^B	�wB	�}B	��B	��B	ÖB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�)B	�/B	�/B	�5B	�;B	�BB	�BB	�BB	�HB	�HB	�HB	�NB	�NB	�TB	�ZB	�ZB	�`B	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
%B
%B
+B
+B
1B
	7B
VB
\B
\B
\B
\B
\B
\B
\B
\B
\B
\B
\B
bB
bB
hB
hB
hB
oB
oB
oB
oB
oB
oB
oB
oB
oB
uB
�B
&�B
6F11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.13 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140809                              AO  ARCAADJP                                                                    20181024140809    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140809  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140809  QCF$                G�O�G�O�G�O�0               