CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-03-20T02:15:22Z AOML 3.0 creation; 2016-06-01T00:08:29Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20160320021522  20160531170829  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4055_7112_145                   2C  D   APEX                            5374                            041511                          846 @מ3З��1   @מ4f�#,@:�$�/�c���$�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�ff@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDyl�D��fD�FfD��fD���D��D�@ D��fD�ɚD��D�I�D���D�ɚD���D�<�DچfD�ٚD�	�D�6fD�p D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��\@���A{A&{AF{Af{A�
=A�
=A�
=A�
=A�
=A�
=A�
=A�
=B�B	�B�B�B!�B)�B1�B9�BA�BI�BQ�BY�Ba�Bi�Bq�By�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C aHCaHCaHCaHCaHC
aHCaHCaHCaHCaHCaHCaHCaHCaHCaHCaHC aHC"aHC$aHC&aHC(aHC*aHC,aHC.aHC0aHC2aHC4aHC6aHC8aHC:aHC<aHC>aHC@aHCBaHCDaHCFaHCHaHCJaHCLaHCNaHCPaHCRaHCTaHCVaHCXaHCZaHC\aHC^aHC`aHCbaHCdaHCfaHChaHCjaHClaHCnaHCpaHCraHCtaHCvaHCxaHCzaHC|aHC~aHC�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�#�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�D RD �RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD	RD	�RD
RD
�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD RD �RD!RD!�RD"RD"�RD#RD#�RD$RD$�RD%RD%�RD&RD&�RD'RD'�RD(RD(�RD)RD)�RD*RD*�RD+RD+�RD,RD,�RD-RD-�RD.RD.�RD/RD/�RD0RD0�RD1RD1�RD2RD2�RD3RD3�RD4RD4�RD5RD5�RD6RD6�RD7RD7�RD8RD8�RD9RD9�RD:RD:�RD;RD;�RD<RD<�RD=RD=�RD>RD>�RD?RD?�RD@RD@�RDARDA�RDBRDB�RDCRDC�RDDRDD�RDERDE�RDFRDF�RDGRDG�RDHRDH�RDIRDI�RDJRDJ�RDKRDK�RDLRDL�RDMRDM�RDNRDN�RDORDO�RDPRDP�RDQRDQ�RDRRDR�RDSRDS�RDTRDT�RDURDU�RDVRDV�RDWRDW�RDXRDX�RDYRDY�RDZRDZ�RD[RD[�RD\RD\�RD]RD]�RD^RD^�RD_RD_�RD`RD`�RDaRDa�RDbRDb�RDcRDc�RDdRDd�RDeRDe�RDfRDf�RDgRDg�RDhRDh�RDiRDi�RDjRDj�RDkRDk�RDlRDl�RDmRDm�RDnRDn�RDoRDo�RDpRDp�RDqRDq�RDrRDr�RDsRDs�RDtRDt�RDt޸Dy�D��D�R�D���D���D�(�D�L)D���D���D��D�U�D���D���D��D�H�Dڒ�D���D��D�B�D�|)D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A��A��`A��TA��mA��mA��`A��mA��mA��mA��mA��`A��TA��HA��HA��HA��;A���A��FA��RA��\A�x�A�;dA�&�A�"�A�{A��TA���A�5?A�|�A�A���A�dZA��A��+A��A�r�A�dZA�$�A�JA��A��jA��DA�t�A�O�A��A�^5A�I�A��A�v�A�A�hsA�?}A���A��\A�A���A�/A�K�A���A�oA��;A�5?A��DA��RA�bA�oA�hsA�%A��A�A�?}A���A���A��A��A�JA�
=A��/A� �A�M�A��A�hsA��A�ĜA��jA�"�A�PA~�jA~�A}|�A}O�A}7LA}&�A|��A|�A{hsAz�AxQ�Aw�AvI�Au�^AtbAq�Am�Ak��Ai"�Af��Af �Ac��Ab��A`�A^ĜA^JA^�\A^n�A]?}A\�A[l�A[�AZv�AY�^AW�AV�9AV�AU�hAT��ATffAT  ASG�ARv�AQ��AP�API�AOC�ANffAM�AL�AJbNAI�
AI�hAHI�AG�FAF�AE��AE/AD�\AC��ACdZAB�A@ĜA@M�A?��A>��A=hsA<��A<A;��A:ȴA9XA8�+A8ffA5�;A3�TA3��A3+A29XA1dZA0�9A0r�A0�A/G�A.�yA.ȴA.��A.��A.~�A-�PA,��A,ZA,VA,M�A,�A+�TA+�A*�uA(��A'"�A&JA%7LA$�RA$�A"�A �HAXA�DA�AO�A~�A�AA`BA�A�9An�A{A�hA�A��An�A1A`BAJA33AZA�hAoA�\A�A��A�RA-AoAbA�A
�\A
A�A	�A�FA��A��AbNAQ�A5?A1Ax�A�AA�A�jAI�A��A �A 9X@���@�M�@�z�@���@��
@��7@��`@���@�bN@�@���@�b@�33@�;d@�;d@�+@�n�@�hs@��`@��@�Q�@��;@�+@�@�Ĝ@��@ۅ@ڧ�@�@ّh@ؓu@�ƨ@ו�@���@��@Ցh@�&�@��@�/@�Q�@�1'@Ώ\@���@��@��@ɉ7@Ǯ@��T@�X@��@ă@�r�@�j@�9X@�b@§�@�x�@��@��9@�1'@��;@���@��@�@���@��-@���@�I�@��@��w@���@�l�@���@���@���@���@�/@�%@�G�@�I�@���@�^5@��@�z�@��@�  @�l�@�^5@�?}@�1'@�"�@��y@�ȴ@��\@���@�hs@�%@�Ĝ@��@�(�@�l�@��H@��\@���@��/@�(�@�l�@�V@�/@��@��D@���@��@��!@�V@��@���@�1@��;@��@�K�@�{@�O�@���@��
@�J@��@�9X@��u@��7@��@��9@�o@���@�E�@�J@�M�@���@��!@�M�@��@��@�7L@��/@�Z@���@�o@�J@��h@��h@�?}@��@��9@��@�A�@�l�@�o@�ȴ@�p�@�V@��@���@�Ĝ@�Ĝ@��9@��u@�j@�9X@� �@�b@�t�@�"�@�o@�
=@��@��@�@��y@��H@���@���@���@�~�@�V@�M�@�=q@�$�@�{@��@��@�{@�J@��@���@���@�x�@�7L@�V@���@��j@��@�A�@���@���@��P@��P@��@�t�@�"�@�@�@��H@�~�@�M�@�5?@�-@�@��@��T@��#@��#@���@���@�G�@���@��`@���@���@�Z@�1@�;@��@K�@~��@~��@~E�@~@}��@}�h@}V@|�@|9X@{�
@z�!@yhs@qX@hr�@_��@V��@P�9@H�9@A�@9�7@3"�@-�h@(Q�@"J@1@G�@(�@A�@t�@	�^@$�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A��A��A��A��`A��TA��mA��mA��`A��mA��mA��mA��mA��`A��TA��HA��HA��HA��;A���A��FA��RA��\A�x�A�;dA�&�A�"�A�{A��TA���A�5?A�|�A�A���A�dZA��A��+A��A�r�A�dZA�$�A�JA��A��jA��DA�t�A�O�A��A�^5A�I�A��A�v�A�A�hsA�?}A���A��\A�A���A�/A�K�A���A�oA��;A�5?A��DA��RA�bA�oA�hsA�%A��A�A�?}A���A���A��A��A�JA�
=A��/A� �A�M�A��A�hsA��A�ĜA��jA�"�A�PA~�jA~�A}|�A}O�A}7LA}&�A|��A|�A{hsAz�AxQ�Aw�AvI�Au�^AtbAq�Am�Ak��Ai"�Af��Af �Ac��Ab��A`�A^ĜA^JA^�\A^n�A]?}A\�A[l�A[�AZv�AY�^AW�AV�9AV�AU�hAT��ATffAT  ASG�ARv�AQ��AP�API�AOC�ANffAM�AL�AJbNAI�
AI�hAHI�AG�FAF�AE��AE/AD�\AC��ACdZAB�A@ĜA@M�A?��A>��A=hsA<��A<A;��A:ȴA9XA8�+A8ffA5�;A3�TA3��A3+A29XA1dZA0�9A0r�A0�A/G�A.�yA.ȴA.��A.��A.~�A-�PA,��A,ZA,VA,M�A,�A+�TA+�A*�uA(��A'"�A&JA%7LA$�RA$�A"�A �HAXA�DA�AO�A~�A�AA`BA�A�9An�A{A�hA�A��An�A1A`BAJA33AZA�hAoA�\A�A��A�RA-AoAbA�A
�\A
A�A	�A�FA��A��AbNAQ�A5?A1Ax�A�AA�A�jAI�A��A �A 9X@���@�M�@�z�@���@��
@��7@��`@���@�bN@�@���@�b@�33@�;d@�;d@�+@�n�@�hs@��`@��@�Q�@��;@�+@�@�Ĝ@��@ۅ@ڧ�@�@ّh@ؓu@�ƨ@ו�@���@��@Ցh@�&�@��@�/@�Q�@�1'@Ώ\@���@��@��@ɉ7@Ǯ@��T@�X@��@ă@�r�@�j@�9X@�b@§�@�x�@��@��9@�1'@��;@���@��@�@���@��-@���@�I�@��@��w@���@�l�@���@���@���@���@�/@�%@�G�@�I�@���@�^5@��@�z�@��@�  @�l�@�^5@�?}@�1'@�"�@��y@�ȴ@��\@���@�hs@�%@�Ĝ@��@�(�@�l�@��H@��\@���@��/@�(�@�l�@�V@�/@��@��D@���@��@��!@�V@��@���@�1@��;@��@�K�@�{@�O�@���@��
@�J@��@�9X@��u@��7@��@��9@�o@���@�E�@�J@�M�@���@��!@�M�@��@��@�7L@��/@�Z@���@�o@�J@��h@��h@�?}@��@��9@��@�A�@�l�@�o@�ȴ@�p�@�V@��@���@�Ĝ@�Ĝ@��9@��u@�j@�9X@� �@�b@�t�@�"�@�o@�
=@��@��@�@��y@��H@���@���@���@�~�@�V@�M�@�=q@�$�@�{@��@��@�{@�J@��@���@���@�x�@�7L@�V@���@��j@��@�A�@���@���@��P@��P@��@�t�@�"�@�@�@��H@�~�@�M�@�5?@�-@�@��@��T@��#@��#@���@���@�G�@���@��`@���@���@�Z@�1@�;@��@K�@~��@~��@~E�@~@}��@}�h@}V@|�@|9X@{�
G�O�@yhs@qX@hr�@_��@V��@P�9@H�9@A�@9�7@3"�@-�h@(Q�@"J@1@G�@(�@A�@t�@	�^@$�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB?}B?}B?}B?}B?}B?}B?}B?}B?}B?}B?}B?}B?}B?}B?}B?}B?}B?}B?}B@�B@�B@�B@�B>wB=qB<jB;dB9XB7LB:^B<jB2-B+B,B.B)�B�B�B�B�B�B�B�B�B�B�B�BPB��B%BB�B�B�B��B��B�}B�B�Bw�Bn�BM�B�B
=B��B�B�HB��B��B�RB�B��B{�BJ�B �B�BJB
��B
�fB
�B
�PB
|�B
s�B
gmB
VB
R�B
I�B
L�B
M�B
K�B
I�B
J�B
J�B
L�B
M�B
O�B
N�B
J�B
E�B
J�B
C�B
<jB
5?B
%�B
oB	��B	�ZB	��B	�?B	�B	��B	�hB	�1B	u�B	u�B	�JB	�bB	�DB	�B	~�B	|�B	{�B	w�B	u�B	o�B	l�B	hsB	dZB	aHB	_;B	XB	R�B	N�B	J�B	F�B	A�B	:^B	2-B	-B	�B	&�B	&�B	#�B	$�B	�B	�B	uB	bB	PB		7B	B��B��B��B��B�B�B�B�sB�fB�TB�5B�B��B��B��B��B��BȴBŢBÖB��B�wB�wB�qB�qB�jB�dB�XB�RB�LB�LB�LB�LB�FB�?B�!B�B��B��B��B��B��B�oB�PB�7B�%B�B�B~�B|�B{�By�Bx�Bw�Bv�Bt�Bs�Bq�Bp�Bo�Bm�Bk�BgmBffBdZBbNB^5B[#BYBW
BVBS�BO�BK�BJ�BI�BG�BD�BA�B@�B?}B?}B>wB>wB<jB<jB:^B9XB8RB7LB6FB5?B49B33B2-B0!B/B,B)�B&�B%�B%�B#�B"�B$�B%�B&�B&�B&�B%�B%�B&�B&�B&�B&�B&�B&�B'�B(�B(�B(�B(�B(�B(�B+B,B+B+B(�B(�B'�B%�B'�B(�B&�B'�B'�B(�B'�B'�B+B/B0!B1'B2-B2-B2-B1'B0!B33B5?B6FB7LB8RB:^B<jB?}BB�BK�BP�BQ�BR�BR�BT�BW
B\)B^5B\)BXBW
BW
BW
B\)B\)B[#BZBXBW
BVBT�BT�BW
B[#B^5BcTBn�Bs�Bs�Bt�Bt�Bu�Bv�Bv�Bw�By�B{�B{�B}�B� B� B�B�B�+B�VB�uB��B��B��B��B��B��B��B��B��B��B�B�3B�?B�9B�-B�'B�3B�^BÖBƨBȴB��B��B��B��B��B�B�)B�5B�/B�/B�;B�HB�yB�B�B�B�B��B��B��B��B��B��B	B	B	B	PB	hB	{B	�B	�B	�B	�B	�B	�B	�B	�B	%�B	.B	/B	0!B	1'B	2-B	49B	5?B	7LB	7LB	8RB	:^B	;dB	<jB	>wB	?}B	?}B	?}B	@�B	@�B	A�B	A�B	B�B	B�B	E�B	G�B	I�B	L�B	L�B	M�B	O�B	Q�B	T�B	W
B	ZB	ZB	ZB	ZB	[#B	]/B	^5B	_;B	`BB	cTB	e`B	ffB	gmB	hsB	iyB	jB	jB	jB	jB	k�B	p�B	s�B	t�B	u�B	w�B	y�B	}�B	~�B	�B	�B	�B	�%B	�1B	�7B	�=B	�DB	�VB	�\B	�hB	�uB	��B	��B	��B	�)B	�B
1B
bB
�B
%�B
2-B
:^B
@�B
G�B
O�B
W
B
[#B
`BB
e`B
l�B
n�B
r�B
w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B?ZB?ZB?]B?ZB?]B?]B?]B?ZB?_B?]B?]B?]B?ZB?]B?]B?]B?]B?]B?ZB@dB@dB@aB@cB>XB=TB<JB;CB96B7*B:<B<HB2B*�B+�B-�B)�B�BaB`B{BxBB�B�B�B�B}B-B��BB�B�B�fB��BϽBʠB�XB��B��Bw�BnwBM�BoB
B��B�sB�#BͯB�fB�,B��B��B{�BJ�B �B_B(B
��B
�AB
��B
�-B
|�B
s�B
gMB
U�B
R�B
I�B
L�B
M�B
K�B
I�B
J�B
J�B
L�B
M�B
O�B
N�B
J�B
E}B
J�B
CuB
<JB
5B
%�B
LB	��B	�;B	˨B	�!B	��B	�~B	�KB	�B	u�B	u�B	�-B	�GB	�&B	��B	~�B	|�B	{�B	w�B	u�B	o�B	lnB	hVB	d<B	a+B	_ B	W�B	R�B	N�B	J�B	F�B	ApB	:AB	2B	,�B	�B	&�B	&�B	#�B	$�B	�B	yB	^B	IB	6B		B	�B��B��B��B��B�B�xB�fB�YB�MB�<B�B�B��B��B��B͹BʫBȟBŊB�B�qB�_B�\B�XB�ZB�SB�NB�AB�:B�7B�7B�5B�7B�-B�+B�B��B��B��B��B��B�vB�ZB�:B�!B�B�B��B~�B|�B{�By�Bx�Bw�Bv�Bt�Bs�Bq�Bp�Bo�Bm|BkmBgXBfQBdCBb9B^ B[BYBV�BU�BS�BO�BK�BJ�BI�BG�BD�BArB@nB?iB?jB>bB>cB<UB<UB:IB9CB8;B75B6/B5+B4%B3B1�B0B/B+�B)�B&�B%�B%�B#�B"�B$�B%�B&�B&�B&�B%�B%�B&�B&�B&�B&�B&�B&�B'�B(�B(�B(�B(�B(�B(�B*�B+�B*�B*�B(�B(�B'�B%�B'�B(�B&�B'�B'�B(�B'�B'�B*�B/B0B1B2B2B2B1B0	B3B5(B6-B72B8:B:GB<QB?eBBvBK�BP�BQ�BR�BR�BT�BV�B\B^B\BW�BV�BV�BV�B\B\B[BZBW�BV�BU�BT�BT�BV�B[	B^Bc;Bn{Bs�Bs�Bt�Bt�Bu�Bv�Bv�Bw�By�B{�B{�B}�B�B�B��B��B�B�7B�VB�hB�yB��B��B��B��B��B��B��B��B��B�B�"B�B�B�B�B�>B�xBƆBȒB˦B̭BͱB��B��B��B�B�B�B�B�B�&B�VB�_B�[B�uB�B��B��B��B��B��B��B	 �B	�B	�B	-B	DB	WB	\B	\B	\B	ZB	bB	iB	xB	�B	%�B	-�B	.�B	/�B	1 B	2B	4B	5B	7'B	7'B	8.B	:6B	;>B	<DB	>RB	?WB	?YB	?VB	@]B	@]B	AdB	AdB	BhB	BjB	E}B	G�B	I�B	L�B	L�B	M�B	O�B	Q�B	T�B	V�B	Y�B	Y�B	Y�B	Y�B	Z�B	]	B	^B	_B	`B	c-B	e7B	f>B	gFB	hLB	iQB	jXB	jVB	jXB	jVB	k_B	p{B	s�B	t�B	u�B	w�B	y�B	}�B	~�B	��B	��B	��B	��B	�	B	�B	�B	�B	�,B	�5B	�AB	�LG�O�B	��B	�YB	��B	�B
B
5B
B
%�B
2 B
:2B
@VB
G�B
O�B
V�B
Z�B
`B
e3B
l_B
njB
r�B
w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.38 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708292016053117082920160531170829  AO  ARCAADJP                                                                    20160320021522    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160320021522  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160320021522  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170829  IP                  G�O�G�O�G�O�                