CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T04:15:22Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7,   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  74   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7t   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     88   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8X   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8x   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8|   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qt   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �\   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �P   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �$   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �(   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20181121041522  20190604095301  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4055                            2C  D   APEX                            5374                            041511                          846 @��o7�`>1   @��oǮ,$@9l1&�y�c�O�;d1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C�C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp�fDqfDq� Dr  Dr� Ds  Ds� Dt  Dty�Dy�
D�
D�L�D��
D���D��D�P�D��{D��RD��D�S�D���D��qD��fD�AHDڀ�D��3D���D�6fD�o\D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�(�@�(�A{A&{AF{Af{A�
=A�
=A�
=A�
=A�
=A�
=A�
=A�
=B�B	�B�B�B!�B)�B1�B9�BA�BI�BQ�BY�Ba�Bi�Bq�By�B���B�B�B�B�B�B�B�B��\B�\)B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C aHCaHCaHCaHCaHC
aHCaHCaHCz�CaHCaHCaHCaHCaHCaHCaHC aHC"aHC$aHC&aHC(aHC*aHC,aHC.aHC0aHC2aHC4aHC6aHC8aHC:aHC<aHC>aHC@aHCBaHCDaHCFaHCHaHCJaHCLaHCNaHCPaHCRaHCTaHCVaHCXaHCZaHC\aHC^aHC`aHCbaHCdaHCfaHChaHCjaHClaHCnaHCpaHCraHCtaHCvaHCxaHCzaHC|aHC~aHC�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�=qC�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�D RD �RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD	RD	�RD
RD
�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD RD �RD!RD!�RD"RD"�RD#RD#�RD$RD$�RD%RD%�RD&RD&�RD'RD'�RD(RD(�RD)RD)�RD*RD*�RD+RD+�RD,RD,�RD-RD-�RD.RD.�RD/RD/�RD0RD0�RD1RD1�RD2RD2�RD3RD3�RD4RD4�RD5RD5�RD6RD6�RD7RD7�RD8RD8�RD9RD9�RD:RD:�RD;RD;�RD<RD<�RD=RD=�RD>RD>�RD?RD?�RD@RD@�RDARDA�RDBRDB�RDCRDC�RDDRDD�RDERDE�RDFRDF�RDGRDG�RDHRDH�RDIRDI�RDJRDJ�RDKRDK�RDLRDL�RDMRDM�RDNRDN�RDORDO�RDPRDP�RDQRDQ�RDRRDR�RDSRDS�RDTRDT�RDURDU�RDVRDV�RDWRDW�RDXRDX�RDYRDY�RDZRDZ�RD[RD[�RD\RD\�RD]RD]�RD^RD^�RD_RD_�RD`RD`�RDaRDa�RDbRDb�RDcRDc�RDdRDd�RDeRDe�RDfRDf�RDgRDg�RDhRDh�RDiRDi�RDjRDj�RDkRDk�RDlRDl�RDmRDm�RDnRDn�RDoRDo�RDpRDp��Dq�Dq�RDrRDr�RDsRDs�RDtRDt��Dy�\D�+3D�X�D��3D��D�D�]D���D��{D�#�D�` D���D�њD��D�MqDڌ�D��\D��D�B�D�{�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�  A���A���A��A��A��A��A��A��mA��`A�ȴA˲-A˧�A�v�A�r�A�x�A˛�A˰!A˲-A˧�A˓uA�x�A�ffA�O�A�-A�"�A�{A���A��TA��
A���Aʰ!Aʣ�Aʉ7A�(�Aɩ�AȓuA�
=A�^5A��A���A�~�A�jA���A�r�A�r�A��#A�bNA�9XA��
A�-A���A�\)A���A�"�A��A�jA��
A�p�A���A��HA�ffA���A�+A�ȴA�hsA� �A��jA��mA���A��A�VA��hA��uA��A�p�A��A�
=A��FA�5?A��9A�%A�7LA�ĜA�bA�hsA��A���A��`A�bA�r�A�^5A�;dA�S�A�Q�A�l�A�x�A���A���A��#A�;dA���A��9A��/A��PA�+A���A�l�A�|�A���A���A�$�A��uA�
=A���A�&�A�`BA�bNA�O�A�XA���A���A��DAC�A}\)A|�`A|ĜA|bNA{�hAy�Awl�AuK�As��Ar�RAq��ApȴAo�An��AmO�AlM�AkAkG�Aj1Ai�Ai"�Ah=qAh�Ag�;Af�HAel�Ad-AbĜAa\)A` �A_�A^5?A\��A[/AZ5?AXȴAWt�AV{ATn�AR��APr�ANr�AM7LAL1'ALAK��AJ-AI;dAH��AF�RAF9XAD�AC��AB��AAXA?33A>VA=��A<��A;�TA;��A;;dA:z�A8�A6�A5�A5C�A3�A2ĜA1XA/�#A.��A-|�A,�yA,Q�A+A(z�A'�7A&��A%dZA$�`A$$�A"��A"=qA!�wA z�A bA�A�TAS�A��AffA��A
=A�^A"�A��A�RA5?A��A��AhsA��A7LAz�A1A��Ap�AG�AoAȴAM�A�7A��A�jAJA?}A�A�DA��A
�/A	��AĜA�;A��A�FA+AoA��A�\A�A ĜA r�@���@�o@���@���@�33@���@��@�j@�
=@�-@�/@�@�I�@�F@�@�t�@�@���@�S�@�@웦@��@�&�@���@���@�@��@�x�@�|�@�!@�$�@�p�@�"�@ܴ9@�l�@ى7@�bN@�v�@�X@���@��H@�$�@�9X@�\)@�o@��@�O�@��@̃@���@ʏ\@�Q�@�5?@��@�+@���@�7L@�  @�n�@���@��h@��@�=q@�`B@�V@��@��@��+@�~�@��^@���@��@�33@�x�@�A�@��
@���@�/@�%@�b@�t�@�V@�O�@��@�;d@�J@��^@�G�@��@��@�z�@�ƨ@���@�E�@��T@�O�@��u@��
@��@�v�@�^5@��#@��9@�1'@� �@�ƨ@�ȴ@���@���@��@���@���@��+@��@�n�@�7L@�j@��
@�\)@��@���@���@��@�G�@�x�@�hs@�O�@�%@��@��@��F@�@�v�@��^@��@�&�@���@�I�@�b@��
@��@�t�@�S�@�\)@��y@�v�@�@��T@���@���@�x�@�G�@��@���@��D@��D@���@��u@��9@� �@� �@���@�
=@�@���@���@���@�/@��`@�Ĝ@�Ĝ@�z�@�Z@�Q�@�1@���@�K�@��y@��+@�E�@���@��-@�@���@��-@�7L@��/@��`@�G�@���@���@��u@�r�@�j@�I�@��@�o@��H@���@�-@��@��^@���@���@��7@�`B@�&�@�%@��9@��D@��D@��@�z�@�j@�bN@�bN@�Z@�I�@�A�@�A�@�1'@�(�@�@�@
=@}�h@}`B@}?}@}V@|�/@|�j@zȴ@sj�@j+k@d@^�@W�
@P�@IVm@B	@;l�@4�4@.q�@(��@#/�@$t@�@	�@S@L0@	��@҉11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�  A���A���A��A��A��A��A��A��mA��`A�ȴA˲-A˧�A�v�A�r�A�x�A˛�A˰!A˲-A˧�A˓uA�x�A�ffA�O�A�-A�"�A�{A���A��TA��
A���Aʰ!Aʣ�Aʉ7A�(�Aɩ�AȓuA�
=A�^5A��A���A�~�A�jA���A�r�A�r�A��#A�bNA�9XA��
A�-A���A�\)A���A�"�A��A�jA��
A�p�A���A��HA�ffA���A�+A�ȴA�hsA� �A��jA��mA���A��A�VA��hA��uA��A�p�A��A�
=A��FA�5?A��9A�%A�7LA�ĜA�bA�hsA��A���A��`A�bA�r�A�^5A�;dA�S�A�Q�A�l�A�x�A���A���A��#A�;dA���A��9A��/A��PA�+A���A�l�A�|�A���A���A�$�A��uA�
=A���A�&�A�`BA�bNA�O�A�XA���A���A��DAC�A}\)A|�`A|ĜA|bNA{�hAy�Awl�AuK�As��Ar�RAq��ApȴAo�An��AmO�AlM�AkAkG�Aj1Ai�Ai"�Ah=qAh�Ag�;Af�HAel�Ad-AbĜAa\)A` �A_�A^5?A\��A[/AZ5?AXȴAWt�AV{ATn�AR��APr�ANr�AM7LAL1'ALAK��AJ-AI;dAH��AF�RAF9XAD�AC��AB��AAXA?33A>VA=��A<��A;�TA;��A;;dA:z�A8�A6�A5�A5C�A3�A2ĜA1XA/�#A.��A-|�A,�yA,Q�A+A(z�A'�7A&��A%dZA$�`A$$�A"��A"=qA!�wA z�A bA�A�TAS�A��AffA��A
=A�^A"�A��A�RA5?A��A��AhsA��A7LAz�A1A��Ap�AG�AoAȴAM�A�7A��A�jAJA?}A�A�DA��A
�/A	��AĜA�;A��A�FA+AoA��A�\A�A ĜA r�@���@�o@���@���@�33@���@��@�j@�
=@�-@�/@�@�I�@�F@�@�t�@�@���@�S�@�@웦@��@�&�@���@���@�@��@�x�@�|�@�!@�$�@�p�@�"�@ܴ9@�l�@ى7@�bN@�v�@�X@���@��H@�$�@�9X@�\)@�o@��@�O�@��@̃@���@ʏ\@�Q�@�5?@��@�+@���@�7L@�  @�n�@���@��h@��@�=q@�`B@�V@��@��@��+@�~�@��^@���@��@�33@�x�@�A�@��
@���@�/@�%@�b@�t�@�V@�O�@��@�;d@�J@��^@�G�@��@��@�z�@�ƨ@���@�E�@��T@�O�@��u@��
@��@�v�@�^5@��#@��9@�1'@� �@�ƨ@�ȴ@���@���@��@���@���@��+@��@�n�@�7L@�j@��
@�\)@��@���@���@��@�G�@�x�@�hs@�O�@�%@��@��@��F@�@�v�@��^@��@�&�@���@�I�@�b@��
@��@�t�@�S�@�\)@��y@�v�@�@��T@���@���@�x�@�G�@��@���@��D@��D@���@��u@��9@� �@� �@���@�
=@�@���@���@���@�/@��`@�Ĝ@�Ĝ@�z�@�Z@�Q�@�1@���@�K�@��y@��+@�E�@���@��-@�@���@��-@�7L@��/@��`@�G�@���@���@��u@�r�@�j@�I�@��@�o@��H@���@�-@��@��^@���@���@��7@�`B@�&�@�%@��9@��D@��D@��@�z�@�j@�bN@�bN@�Z@�I�@�A�@�A�@�1'@�(�@�@�@
=@}�h@}`B@}?}@}V@|�/G�O�@zȴ@sj�@j+k@d@^�@W�
@P�@IVm@B	@;l�@4�4@.q�@(��@#/�@$t@�@	�@S@L0@	��@҉11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBuBuBuBoBoBoBuBoBhBhBbBVBPBDBDBoB$�B/B>wBB�BH�BL�BO�BR�BS�BT�BVBVBVBVBW
BYB[#B]/BaHBr�B}�B�BffB{�B�3B��B�B��B��B�/B�;B�TB�B�yB�fB�HB�ZB�mB�sB�yB�yB�B�B�yB�;B�5B�NB��BɺBĜBBƨBǮB�jB�FB�B��B��B��B�bB�%Bw�Bq�BjBbNBXBM�BG�BG�BXBdZB\)BO�B>wB�BDB��B��B�B�;B��BB�dB�B��B��B�XB�!B�B�B��B�bBq�By�Bm�BdZB]/BR�BL�B@�B33B�B	7B
�mB
��B
��B
�^B
�B
��B
��B
��B
�hB
�=B
y�B
bNB
L�B
?}B
9XB
33B
(�B
 �B
�B
\B

=B
+B
%B
B	��B	��B	��B	��B	��B	�B	�fB	�;B	�B	��B	ǮB	��B	�^B	�B	��B	��B	��B	�PB	�B	t�B	ffB	Q�B	E�B	A�B	?}B	=qB	9XB	1'B	-B	'�B	�B	�B	\B	+B��B�B�fB�NB�5B�B�
B��B�B�BǮB�qB�XB�LB�9B�'B�B��B��B��B��B��B�oB�+B�B�B}�B{�Bx�Bu�Bt�Br�Bq�Bn�Bk�BjBiyBhsBffBdZBbNB`BB_;B_;B^5B[#BYBYBW
BR�BQ�BP�BO�BN�BN�BM�BM�BL�BK�BI�BH�BG�BE�BC�BB�B@�B>wB;dB8RB6FB49B1'B/B/B.B-B(�B%�B#�B"�B!�B�B�B�B�B�B�B�B�B{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BuBuBuBuBuBhBbBhBbB\B\B\B\BbBbBbBhBoBhBuBuBuBuBuBuBoBoB�B�B�B�B�B�B�B�B!�B$�B$�B$�B"�B(�B/B2-B49B9XB>wB=qB9XB:^B;dB9XB8RB7LB9XB9XB8RB8RB:^B;dB>wB@�BC�BE�BG�BI�BL�BN�BN�BN�BO�BP�BO�BO�BR�BVBVBVBZB_;BdZBffBgmBgmBffBgmBm�Bm�Bo�Bw�Bw�Bz�B|�B~�B�B�B�+B�VB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�'B�-B�?B�RB�^B�dB�qB�}B��BĜBɺB��B��B��B��B�
B�B�B�/B�5B�;B�;B�ZB�`B�mB�yB�B�B�B�B�B�B��B��B��B	  B	B	B		7B	DB	hB	�B	�B	�B	�B	!�B	"�B	$�B	)�B	+B	,B	/B	1'B	1'B	1'B	1'B	2-B	2-B	33B	5?B	8RB	:^B	;dB	<jB	=qB	@�B	B�B	D�B	G�B	L�B	M�B	N�B	O�B	Q�B	Q�B	R�B	S�B	S�B	T�B	VB	W
B	YB	_;B	cTB	e`B	iyB	k�B	m�B	m�B	m�B	n�B	|�B	�2B	�B	�NB	�|B
�B
�B
�B
&�B
1�B
9	B
BuB
G�B
MB
S�B
XB
_!B
gRB
mB
s�B
x11111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   BVBVBVBPBPBPBVBPBIBFBCB7B.B"B"BPB$�B.�B>VBBpBH�BL�BO�BR�BS�BT�BU�BU�BU�BU�BV�BX�B[B]Ba$Br�B}�B��BfHB{�G�O�B��B��BηB��B�B�B�4B�^B�UB�GB�$B�8B�JB�SB�WB�ZB�xB�hB�SB�B�B�.B��BɖB�}B�nBƉBǍB�IB�#B��B��B��B�mB�@B�Bw�Bq�Bj[Bb/BW�BM�BG�BG�BW�Bd8B\BO�B>QB�B B��B��B�iB�BβB�jB�@B��B��B��B�4B��B��B��B�zB�=Bq�By�BmmBd3B]BR�BL�B@[B3B�B	B
�GB
оB
ͯB
�9B
��B
��B
�tB
�bB
�AB
�B
y�B
b(B
L�B
?WB
92B
3
B
(�B
 �B
gB
5B

B
B
�B
�B	��B	��B	��B	��B	��B	�pB	�?B	�B	��B	϶B	ǇB	�\B	�7B	��B	��B	�rB	�cB	�'B	��B	t�B	f=B	Q�B	EwB	A_B	?TB	=IB	9/B	0�B	,�B	'�B	�B	}B	1B	B��B�B�=B�#B�B��B��B��B��B��BǅB�JB�-B�B�B��B��B��B�|B��B�|B�sB�EB��B��B��B}�B{�Bx�Bu�Bt�Br�Bq�BnmBkZBjTBiLBhFBf:Bd+Bb"B`B_B_B^	BZ�BX�BX�BV�BR�BQ�BP�BO�BN�BN�BM�BM�BL�BK�BI�BH�BG�BEuBChBBbB@VB>JB;7B8$B6B4B0�B.�B.�B-�B,�B(�B%�B#�B"�B!�B�B�B~ByBqBeBXBQBLBNB^BeBiBjBkBdB_BWBPBQBTBRBVBRBQBEBEBHBHBCB8B2B8B3B,B-B.B,B5B3B2B9B@B8BEBDBFBEBDBGB>B=BUB[BVBVB]B_BZBpB!�B$�B$�B$�B"�B(�B.�B1�B4B9*B>EB=AB9*B:0B;1B9(B8#B7B9)B9(B8"B8!B:.B;4B>HB@RBCeBEtBG}BI�BL�BN�BN�BN�BO�BP�BO�BO�BR�BU�BU�BU�BY�B_Bd*Bf4Bg9Bg;Bf5Bg=Bm_Bm^BolBw�Bw�Bz�B|�B~�B��B��B��B�$B�OB�mB�kB�uB�uB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B�B� B�+B�2B�@B�JB�QB�jBɆB͠BΧBΦB��B��B��B��B��B�B�
B�B�(B�-B�;B�GB�LB�RB�ZB�eB�B�B�B��B��B��B	�B	�B		B	B	:B	OB	[B	uB	�B	!�B	"�B	$�B	)�B	*�B	+�B	.�B	0�B	0�B	0�B	0�B	1�B	1�B	3B	5B	8 B	:*B	;3B	<7B	=@B	@QB	B_B	DjB	GzB	L�B	M�B	N�B	O�B	Q�B	Q�B	R�B	S�B	S�B	T�B	U�B	V�B	X�B	_	B	c B	e/B	iGB	kSB	m^B	m^B	m_G�O�B	|�B	� B	��B	�B	�KB
�B
�B
�B
&�B
1�B
8�B
BBB
GyB
L�B
SwB
W�B
^�B
g!B
l�B
sPB
w�11111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.38 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201906040953012019060409530120190604095301  AO  ARCAADJP                                                                    20181121041522    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121041522  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121041522  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604095301  IP                  G�O�G�O�G�O�                