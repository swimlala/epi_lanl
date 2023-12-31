CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-09-17T02:15:39Z AOML 3.0 creation; 2016-08-07T21:36:39Z UW 3.1 conversion     
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
resolution        :�o     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150917021539  20160807143639  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               HA   AO  5286_8897_072                   2C  D   APEX                            6531                            072314                          846 @�o��5�A1   @�o�s���@3�t�j~��c&M���1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    HA   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B"  B'33B0  B8  B@  BH  BP  BXffB_��Bg��Bp  Bx  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch�Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DXfDX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dtl�Dys3D��fD�<�D��fD���D�	�D�I�D�vfD���D�	�D�L�D���D���D�3D�` Dړ3D��fD�  D�FfD�c3D�,�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�(�@�(�A{A"{AB{Ab{A�
=A�
=A�
=A�
=A�
=A�
=A�
=A�
=B �B�B�B�B"�B'�RB0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�B�B�B�B�B�B�B�B�B�B�B�B�u�B�u�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�u�B�B�B�B�B�B�B�B�B�\B�B�B�B�B�B�B�B�B�B�B�B�B�B�C !HC!HC!HC!HC!HC
!HC!HC!HC!HC!HC!HC!HC!HC!HC!HC!HC !HC"!HC$!HC&!HC(!HC*!HC,!HC.!HC0!HC2!HC4!HC6!HC8!HC:!HC<!HC>!HC@!HCB!HCD!HCF!HCH!HCJ!HCL!HCN!HCP!HCR!HCT!HCV!HCX!HCZ!HC\!HC^!HC`!HCb!HCd!HCf!HCh:�Cj!HCl!HCn!HCp!HCr!HCt!HCv!HCx!HCz!HC|!HC~!HC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�qC��C��C��C��D RD �RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD	RD	�RD
RD
�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD RD �RD!RD!�RD"RD"�RD#RD#�RD$RD$�RD%RD%�RD&RD&�RD'RD'�RD(RD(�RD)RD)�RD*RD*�RD+RD+�RD,RD,�RD-RD-�RD.RD.�RD/RD/�RD0RD0�RD1RD1�RD2RD2�RD3RD3�RD4RD4�RD5RD5�RD6RD6�RD7RD7�RD8RD8�RD9RD9�RD:RD:�RD;RD;�RD<RD<�RD=RD=�RD>RD>�RD?RD?�RD@RD@�RDARDA�RDBRDB�RDCRDC�RDDRDD�RDERDE�RDFRDF�RDGRDG�RDHRDH�RDIRDI�RDJRDJ�RDKRDK�RDLRDL�RDMRDM�RDNRDN�RDORDO�RDPRDP�RDQRDQ�RDRRDR�RDSRDS�RDTRDT�RDURDU�RDVRDV�RDWRDW�RDX�DX�RDYRDY�RDZRDZ�RD[RD[�RD\RD\�RD]RD]�RD^RD^�RD_RD_�RD`RD`�RDaRDa�RDbRDb�RDcRDc�RDdRDd�RDeRDe�RDfRDf�RDgRDg�RDhRDh�RDiRDi�RDjRDj�RDkRDk�RDlRDl�RDmRDm�RDnRDn�RDoRDo�RDpRDp�RDqRDq�RDrRDr�RDsRDs�RDtRDtuDy{�D��D�@�D���D���D��D�M�D�z�D���D��D�P�D���D���D�\D�d)Dڗ\D�ڏD�)D�J�D�g\D�0�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�A�A�A�A�A�A�A�+A�+A�+A�7A�DA�7A�PA�7A�7A�7A�+A�+A�bNA�ĜA��A�\)A�I�A���AظRA�=qAӡ�A�  A�C�AΡ�A��A��A�-A�r�A�VA�^5Aɕ�A� �A�^5A��yAǟ�A�\)A��A�JA���Aİ!A�(�AA�1A�M�A�E�A��A�ĜA��jA�`BA�^5A�;dA��A��+A���A��A��!A�\)A���A�XA��`A���A��jA�VA���A� �A�JA�5?A�n�A�ZA��yA�A���A��jA��HA�%A��RA�I�A�v�A�7LA��wA��9A�r�A�VA��A�A�A��A�l�A�{A��^A�$�A���A��A���A���A���A��A��A��A�A�I�A��A�"�A�-A��^A�r�A�%A�1A�33A�S�A�(�A�x�A��^A�ĜA|VAw�Av{As\)Aq�7Ao�wAjjAg�Ac�mA`ȴA^{A\M�AY|�AW��AU�AT��AR=qAN��AMp�AL  AJ1'AGO�AE��AEC�AC��AB�A?��A<E�A;hsA;VA:�A9��A8ZA7
=A5�A4�uA3?}A17LA/`BA-dZA,(�A+/A*A(�+A'S�A&�RA&�!A&�A&A%dZA$��A$��A$5?A"  A �!A 5?A$�A9XAbNAVA�A7LA�mAp�An�AG�A1AA1A33A+A	ƨA  A9XA�A��A&�A�FA�PA �HA�A �A�hA|�A
=@�V@��j@��@�5?@���@�@�M�@�@�@�dZ@���A|�@��P@���@��@�ȴ@��@���@���@� �@�"�@���@���@���@�=q@��m@��@��@�/@���@�~�@���@�Q�@���@ꟾ@��@�@ޗ�@�b@��`@�x�@�Z@�@��@���@���@�Z@��@� �@�ƨ@�@�hs@ёh@��
@��H@�|�@с@�$�@�{@և+@���@�p�@ղ-@���@�n�@��/@д9@��@��@�=q@ёh@�~�@�9X@��@���@�p�@�X@�7L@ղ-@�+@�=q@�@�&�@��@���@���@ա�@�j@ӕ�@�K�@�5?@Гu@ϝ�@·+@���@ɑh@�bN@�o@��y@���@���@��@��/@ă@öF@�$�@��^@�=q@�{@�@��@���@�@��T@��@�X@�1'@�I�@�7L@�&�@�z�@�ƨ@��P@���@���@�hs@�x�@��7@�X@���@��@��`@���@�bN@���@�ƨ@���@���@�$�@��@�@�p�@��j@�A�@�@��@��\@�^5@�5?@�J@�@��7@��@��/@��j@��D@�bN@�9X@���@��
@��w@��F@��@���@�|�@�"�@�ff@��#@���@��7@�/@���@�Q�@�(�@�1@���@�+@��@�ȴ@��\@�v�@�$�@��h@��@���@�z�@�1'@��@��@���@���@�ff@�E�@���@�O�@�V@��/@���@�I�@��m@���@�;d@�~�@�M�@��#@�hs@�7L@��@��9@�9X@��
@�33@���@�%@�V@�-@�1@��H@���@��@��u@��T@�{@���@��w@��F@�S�@�"�@��H@�^5@�$�@���@�O�@���@���@�Ĝ@��9@��u@�z�@��@�r�@�9X@��m@��P@�C�@�"�@���@��@���@��R@��\@�5?@��T@���@�p�@��T@�p�@���@�V@�V@��/@�  @�ƨ@��@�I�@���@��@���@��7@�X@��j@�|�@��7@��@�V@���@�p�@�G�@�p�@�hs@�1'@���@�  @�5?@�$�@��@u`B@nȴ@fV@[t�@SdZ@K��@A�^@9X@3�F@-�h@( �@"��@�@J@�/@��@�-@
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�A�A�A�A�A�A�A�+A�+A�+A�7A�DA�7A�PA�7A�7A�7A�+A�+A�bNA�ĜA��A�\)A�I�A���AظRA�=qAӡ�A�  A�C�AΡ�A��A��A�-A�r�A�VA�^5Aɕ�A� �A�^5A��yAǟ�A�\)A��A�JA���Aİ!A�(�AA�1A�M�A�E�A��A�ĜA��jA�`BA�^5A�;dA��A��+A���A��A��!A�\)A���A�XA��`A���A��jA�VA���A� �A�JA�5?A�n�A�ZA��yA�A���A��jA��HA�%A��RA�I�A�v�A�7LA��wA��9A�r�A�VA��A�A�A��A�l�A�{A��^A�$�A���A��A���A���A���A��A��A��A�A�I�A��A�"�A�-A��^A�r�A�%A�1A�33A�S�A�(�A�x�A��^A�ĜA|VAw�Av{As\)Aq�7Ao�wAjjAg�Ac�mA`ȴA^{A\M�AY|�AW��AU�AT��AR=qAN��AMp�AL  AJ1'AGO�AE��AEC�AC��AB�A?��A<E�A;hsA;VA:�A9��A8ZA7
=A5�A4�uA3?}A17LA/`BA-dZA,(�A+/A*A(�+A'S�A&�RA&�!A&�A&A%dZA$��A$��A$5?A"  A �!A 5?A$�A9XAbNAVA�A7LA�mAp�An�AG�A1AA1A33A+A	ƨA  A9XA�A��A&�A�FA�PA �HA�A �A�hA|�A
=@�V@��j@��@�5?@���@�@�M�@�@�@�dZ@���A|�@��P@���@��@�ȴ@��@���@���@� �@�"�@���@���@���@�=q@��m@��@��@�/@���@�~�@���@�Q�@���@ꟾ@��@�@ޗ�@�b@��`@�x�@�Z@�@��@���@���@�Z@��@� �@�ƨ@�@�hs@ёh@��
@��H@�|�@с@�$�@�{@և+@���@�p�@ղ-@���@�n�@��/@д9@��@��@�=q@ёh@�~�@�9X@��@���@�p�@�X@�7L@ղ-@�+@�=q@�@�&�@��@���@���@ա�@�j@ӕ�@�K�@�5?@Гu@ϝ�@·+@���@ɑh@�bN@�o@��y@���@���@��@��/@ă@öF@�$�@��^@�=q@�{@�@��@���@�@��T@��@�X@�1'@�I�@�7L@�&�@�z�@�ƨ@��P@���@���@�hs@�x�@��7@�X@���@��@��`@���@�bN@���@�ƨ@���@���@�$�@��@�@�p�@��j@�A�@�@��@��\@�^5@�5?@�J@�@��7@��@��/@��j@��D@�bN@�9X@���@��
@��w@��F@��@���@�|�@�"�@�ff@��#@���@��7@�/@���@�Q�@�(�@�1@���@�+@��@�ȴ@��\@�v�@�$�@��h@��@���@�z�@�1'@��@��@���@���@�ff@�E�@���@�O�@�V@��/@���@�I�@��m@���@�;d@�~�@�M�@��#@�hs@�7L@��@��9@�9X@��
@�33@���@�%@�V@�-@�1@��H@���@��@��u@��T@�{@���@��w@��F@�S�@�"�@��H@�^5@�$�@���@�O�@���@���@�Ĝ@��9@��u@�z�@��@�r�@�9X@��m@��P@�C�@�"�@���@��@���@��R@��\@�5?@��T@���@�p�@��T@�p�@���@�V@�V@��/@�  @�ƨ@��@�I�@���@��@���@��7@�X@��j@�|�@��7@��@�V@���@�p�@�G�@�p�@�hs@�1'@���G�O�@�5?@�$�@��@u`B@nȴ@fV@[t�@SdZ@K��@A�^@9X@3�F@-�h@( �@"��@�@J@�/@��@�-@
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
ȴB�B �B �B�B�B�B�B�BYB�+B��B�yB��BoB"�B+B8RBF�BN�B_;BgmBm�Br�Bz�B�B�B��B�wBǮB��B��B�;B�`B�B�NB�-B�Bx�B{�B��B\B(�B1'B0!B-B�B$�B9XB>wB@�BK�BP�BN�BG�BA�B?}BG�BA�B<jB,B&�B�BoBVB+B&�B%�B,B2-B0!B(�B$�B�B\BB��B�B�fB�BB�)B�
BÖB�B��B�BgmB6FB�B��B�qB��B�BcTBhB
�wB
�%B
T�B
2-B
 �B
PB	�TB	��B	ɺB	�wB	�3B	��B	|�B	dZB	P�B	@�B	7LB	-B	 �B	�B	bB	+B��B�B�NB�#B��BȴB�}B�dB�RB�3B�!B�B�B�B��B��B��B��B��B��B��B�bB�PB�=B�1B�%B�B�%B�DB�hB��B�!B�FB�RBǮB�
B�/B��B�B�HB�B�B�mB�B�NB��B�RB��B	+B��B�B�NB�jB�-B��B��B�VB�B~�B�+B}�Br�Bs�Bt�B�+B�\B��B��B��B�uB�uB�oB�\B�VB�uB��B��B��B��B��B��B�B�B�B�B�B�B�B	B	B��B�B�BB�B��B	B	
=B	DB	B	B	  B	B	VB	B��B�BBÖB��B	bB	�B	�B	&�B	,B	33B	9XB	6FB	,B	"�B	bB		7B��B�B��B��B��B�/B�B�B	hB	�B	�B	"�B	#�B	�B	�B	�B	�B	"�B	#�B	"�B	+B	8RB	E�B	N�B	I�B	K�B	Q�B	YB	e`B	e`B	l�B	m�B	m�B	n�B	q�B	}�B	~�B	�B	�B	�B	~�B	|�B	|�B	x�B	o�B	m�B	k�B	k�B	n�B	q�B	p�B	r�B	r�B	p�B	m�B	m�B	t�B	w�B	x�B	y�B	y�B	u�B	r�B	t�B	s�B	r�B	u�B	}�B	� B	�B	�B	�%B	�%B	�B	�B	�%B	�1B	�7B	�+B	�+B	�DB	�DB	�DB	�DB	�DB	�DB	�=B	�DB	�DB	�VB	�\B	�bB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�3B	�3B	�9B	�FB	�RB	�XB	�^B	�dB	�jB	�jB	�wB	�wB	�wB	�wB	�qB	�wB	�}B	�}B	B	ŢB	ŢB	ŢB	ĜB	ÖB	ŢB	ƨB	ǮB	ȴB	ȴB	��B	��B	��B	��B	��B	�B	�ZB	�TB	�)B	�B	�BB	�sB	�`B	�#B	�)B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�5B	�5B	�/B	�/B	�/B	�/B	�/B	�/B	�5B	�;B	�ZB	�fB	�B	�yB	�yB	�B	�sB	�sB	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�sB	�mB	�sB	�yB	�B	�B	�B	�yB	�B	��B
B
�B
bB
�B
"�B
-B
2-B
8RB
A�B
I�B
N�B
T�B
ZB
_;B
dZB
gmB
k�B
n�B
s�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
ȪB�B �B �B�B�B~BwBzBYB�!B��B�pB��BbB"�B*�B8GBF�BN�B_1BgbBm�Br�Bz�B�B�B��B�pBǣB��B��B�3B�ZB�wB�GB�%B�Bx�B{�B��BRB(�B1!B0B-B�B$�B9PB>mB@{BK�BP�BN�BG�BA�B?tBG�BA�B<`B, B&�B�BfBLB*�B&�B%�B+�B2'B0B(�B$�B�BRBB��B�B�]B�9B�B� BËB��B��B�BgaB6:B�B��B�aB��B�BcEB`B
�nB
�B
T�B
2&B
 �B
LB	�PB	��B	ɶB	�uB	�0B	��B	|�B	d[B	P�B	@�B	7LB	-B	 �B	�B	dB	.B��B�B�TB�&B��BȻB��B�iB�VB�7B�(B�B�	B�B�B��B��B��B��B��B��B�jB�VB�EB�8B�+B�B�,B�LB�oB��B�'B�KB�VBǰB�B�0B� B�B�JB�B�B�oB�	B�RB��B�UB��B	,B��B�B�PB�mB�0B��B��B�[B�B B�0B}�Br�Bs�Bt�B�1B�cB��B��B��B�yB�xB�uB�bB�[B�{B��B��B��B��B��B��B�B�B�B�B�B�B�B		B	B��B�B�BB�B��B	B	
;B	CB	B	B	  B	B	WB	B��B�EBÙB��B	`B	�B	�B	&�B	,B	30B	9TB	6BB	,B	"�B	bB		6B��B�B��B��B��B�1B�B�B	gB	�B	�B	"�B	#�B	�B	�B	�B	�B	"�B	#�B	"�B	*�B	8MB	E�B	N�B	I�B	K�B	Q�B	YB	e]B	e[B	l�B	m�B	m�B	n�B	q�B	}�B	~�B	�B	�B	�B	~�B	|�B	|�B	x�B	o�B	m�B	k�B	k�B	n�B	q�B	p�B	r�B	r�B	p�B	m�B	m�B	t�B	w�B	x�B	y�B	y�B	u�B	r�B	t�B	s�B	r�B	u�B	}�B	�B	�B	�B	�B	�B	�B	�B	�B	�*B	�0B	�%B	�"B	�?B	�?B	�>B	�>B	�<B	�<B	�7B	�>B	�=B	�RB	�UB	�]B	�`B	�B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	� B	�B	�B	�B	�%B	�,B	�,B	�0B	�@B	�JB	�OB	�VB	�[B	�bB	�`B	�oB	�sB	�pB	�oB	�jB	�qB	�tB	�uB	B	řB	ŘB	ŘB	ēB	ÍB	ŘB	ƟB	ǧB	ȫB	ȭB	˽B	��B	��B	��B	��B	�B	�QB	�JB	� B	�B	�8B	�lB	�UB	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	� B	�-B	�-B	�%B	�&B	�&B	�#B	�(B	�%B	�-B	�3B	�QB	�^B	�tB	�pB	�pB	�zB	�jB	�iB	�{B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�iB	�cB	�jB	�nB	�B	�B	�vB	�pG�O�B	��B
B
}B
WB
�B
"�B
-B
2!B
8FB
A|B
I�B
N�B
T�B
ZB
_/B
dLB
g`B
kyB
n�B
s�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.13 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436392016080714363920160807143639  AO  ARCAADJP                                                                    20150917021539    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150917021539  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150917021539  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143639  IP                  G�O�G�O�G�O�                