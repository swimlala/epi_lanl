CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-02-06T10:17:06Z AOML 3.0 creation; 2016-08-07T21:36:43Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20160206101706  20160807143643  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               cA   AO  5286_8897_099                   2C  D   APEX                            6531                            072314                          846 @ד� mA1   @ד�m�<�@3Rn��O��c<���S�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    cA   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B���B���B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:�C<�C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDyy�D�fD�S3D���D���D�fD�33D��3D��fD�fD�I�D���D�ɚD� D�C3Dڙ�D�ٚD�3D�9�D�fD��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�\)@�(�A{A"{AB{Ab{A�
=A�
=A�
=A�
=A�
=A�
=A�
=A�
=B �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�u�B�u�B�B�B�B�B�\B�\B�B�B�B�B�B�B�B�B�B�B�u�B�u�B�B�B�B�B�B�B�B�B�B�B�B�C !HC!HC!HC!HC!HC
!HC!HC!HC!HC!HC!HC!HC!HC!HC!HC!HC !HC"!HC$!HC&!HC(!HC*!HC,!HC.!HC0!HC2!HC4!HC6!HC8!HC::�C<:�C>!HC@!HCB!HCD!HCF!HCH!HCJ!HCL!HCN!HCP!HCR!HCT!HCV!HCX!HCZ!HC\!HC^!HC`!HCb!HCd!HCf!HCh!HCj!HCl!HCn!HCp!HCr!HCt!HCv!HCx!HCz!HC|!HC~!HC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D RD �RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD	RD	�RD
RD
�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD RD �RD!RD!�RD"RD"�RD#RD#�RD$RD$�RD%RD%�RD&RD&�RD'RD'�RD(RD(�RD)RD)�RD*RD*�RD+RD+�RD,RD,�RD-RD-�RD.RD.�RD/RD/�RD0RD0�RD1RD1�RD2RD2�RD3RD3�RD4RD4�RD5RD5�RD6RD6�RD7RD7�RD8RD8�RD9RD9�RD:RD:�RD;RD;�RD<RD<�RD=RD=�RD>RD>�RD?RD?�RD@RD@�RDARDA�RDBRDB�RDCRDC�RDDRDD�RDERDE�RDFRDF�RDGRDG�RDHRDH�RDIRDI�RDJRDJ�RDKRDK�RDLRDL�RDMRDM�RDNRDN�RDORDO�RDPRDP�RDQRDQ�RDRRDR�RDSRDS�RDTRDT�RDURDU�RDVRDV�RDWRDW�RDXRDX�RDYRDY�RDZRDZ�RD[RD[�RD\RD\�RD]RD]�RD^RD^�RD_RD_�RD`RD`�RDaRDa�RDbRDb�RDcRDc�RDdRDd�RDeRDe�RDfRDf�RDgRDg�RDhRDh�RDiRDi�RDjRDj�RDkRDk�RDlRDl�RDmRDm�RDnRDn�RDoRDo�RDpRDp�RDqRDq�RDrRDr�RDsRDs�RDtRDt�RDtθDy��D��D�W\D���D���D�
�D�7\D��\D�ʏD�
�D�M�D���D���D�)D�G\Dڝ�D���D�\D�=�D�D��\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AȅAȇ+A�|�AȃAȇ+AȃAȍPAȏ\AȍPAȓuAȓuAȗ�Aș�Aȕ�Aȗ�Aȗ�Aș�Aȏ\AȅA�S�A�?}A� �A���A���A�r�A�v�A�`BA��yA���A��
A��A�1'A�M�AÇ+A�1'A��HA�$�A��A�"�A��A��A��A�$�A��A��TA���A�p�A��A�G�A�1'A�jA��hA��hA�S�A�E�A��`A���A��\A���A�A��A���A�  A��A�t�A��wA�A�bNA�`BA�C�A�Q�A�p�A���A��A�=qA���A�5?A���A��A��+A��`A��jA�
=A�
=A��A�v�A|  AtbAq�;Al��AgoAd(�Ab��A`�AW�AP�`AM�TAK\)AJ��AI��AH�AE�AB�AA�PA?+A=t�A;t�A:  A8�`A7p�A5C�A3A2ĜA2M�A2v�A2��A2v�A1�A/��A-O�A*�A)�-A(��A(��A(=qA'`BA&~�A%�A$�A#��A#��A#��A#�A"��A�PAK�A��AG�A�A�yA�/AZA��AbA��AVA��AS�A �A�hA7LAn�A=qA  A`BA&�A�RA�A�AO�AjA�TAC�AA��A�A+AVA��AI�A��A
�HA
��A
�jA	�TA�+A�-A��A�!A$�A��AĜA�-@��@�ff@���@�S�@�1'@�p�@���@��m@�n�@�`B@�b@�ȴ@�h@� �@�x�@畁@��y@柾@��#@�I�@�\)@�V@ߕ�@��@��@ڰ!@�;d@ۅ@���@��T@�`B@ץ�@�J@�O�@ӕ�@ѩ�@��/@���@�/@�?}@���@ЋD@Ͼw@�"�@Χ�@���@�V@̓u@�bN@�ƨ@ɩ�@��`@ȼj@��;@�=q@Ƈ+@Ə\@�`B@��@�  @�ff@��-@���@��7@���@�(�@�S�@���@��#@�X@�G�@�&�@�V@��@�I�@���@�dZ@��y@���@��\@���@���@�^5@�{@�z�@�l�@���@�;d@�M�@��@��^@�`B@���@��D@�A�@��;@�l�@�
=@�~�@�M�@�@�Ĝ@��@�t�@���@��+@�~�@�{@��@��@�Ĝ@�z�@�  @���@�|�@�33@�
=@�o@��H@�v�@�5?@��-@�/@�%@�9X@�t�@�C�@�"�@���@���@���@���@��@�^5@�5?@��^@�X@�Ĝ@��@�\)@���@��@��;@���@�S�@�
=@�n�@���@�&�@��@�j@�t�@��@��!@��@��!@���@�v�@���@�p�@���@��j@���@��D@�Z@�Z@� �@�1@���@�t�@�+@���@���@���@��y@���@���@�J@��7@�G�@�7L@��@��@�r�@�b@�b@�ƨ@��@��@��w@���@�;d@�@��R@�n�@��@���@���@��@���@��u@�Q�@��
@�dZ@��y@��R@�~�@�5?@���@���@�G�@�&�@��@�?}@�/@�%@��@�j@�9X@� �@�1@�1@���@��m@�t�@�"�@�
=@��y@��@�ȴ@���@�ff@�V@�M�@�=q@�-@��@���@���@��^@��h@��7@��7@�hs@�7L@��@��/@��j@��9@���@�Q�@�  @�1@�Z@�A�@�1@���@���@��@�S�@�K�@�;d@��@��R@�-@�@��-@���@��T@�?}@���@��`@��`@��/@��9@�r�@�Q�@�(�@��m@��@�dZ@�"�@��!@��+@�M�@�M�@�E�@�=q@�@��7@�O�@��@���@��/@���@�z�@�A�@�w@�@~{@}��@}�@|�D@|�@{��@z�\@w��@nE�@e�@\z�@S@J�!@D(�@=��@7��@1%@*�H@&5?@�w@9X@  @��@�@��@	�7@�+@=q111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  AȅAȇ+A�|�AȃAȇ+AȃAȍPAȏ\AȍPAȓuAȓuAȗ�Aș�Aȕ�Aȗ�Aȗ�Aș�Aȏ\AȅA�S�A�?}A� �A���A���A�r�A�v�A�`BA��yA���A��
A��A�1'A�M�AÇ+A�1'A��HA�$�A��A�"�A��A��A��A�$�A��A��TA���A�p�A��A�G�A�1'A�jA��hA��hA�S�A�E�A��`A���A��\A���A�A��A���A�  A��A�t�A��wA�A�bNA�`BA�C�A�Q�A�p�A���A��A�=qA���A�5?A���A��A��+A��`A��jA�
=A�
=A��A�v�A|  AtbAq�;Al��AgoAd(�Ab��A`�AW�AP�`AM�TAK\)AJ��AI��AH�AE�AB�AA�PA?+A=t�A;t�A:  A8�`A7p�A5C�A3A2ĜA2M�A2v�A2��A2v�A1�A/��A-O�A*�A)�-A(��A(��A(=qA'`BA&~�A%�A$�A#��A#��A#��A#�A"��A�PAK�A��AG�A�A�yA�/AZA��AbA��AVA��AS�A �A�hA7LAn�A=qA  A`BA&�A�RA�A�AO�AjA�TAC�AA��A�A+AVA��AI�A��A
�HA
��A
�jA	�TA�+A�-A��A�!A$�A��AĜA�-@��@�ff@���@�S�@�1'@�p�@���@��m@�n�@�`B@�b@�ȴ@�h@� �@�x�@畁@��y@柾@��#@�I�@�\)@�V@ߕ�@��@��@ڰ!@�;d@ۅ@���@��T@�`B@ץ�@�J@�O�@ӕ�@ѩ�@��/@���@�/@�?}@���@ЋD@Ͼw@�"�@Χ�@���@�V@̓u@�bN@�ƨ@ɩ�@��`@ȼj@��;@�=q@Ƈ+@Ə\@�`B@��@�  @�ff@��-@���@��7@���@�(�@�S�@���@��#@�X@�G�@�&�@�V@��@�I�@���@�dZ@��y@���@��\@���@���@�^5@�{@�z�@�l�@���@�;d@�M�@��@��^@�`B@���@��D@�A�@��;@�l�@�
=@�~�@�M�@�@�Ĝ@��@�t�@���@��+@�~�@�{@��@��@�Ĝ@�z�@�  @���@�|�@�33@�
=@�o@��H@�v�@�5?@��-@�/@�%@�9X@�t�@�C�@�"�@���@���@���@���@��@�^5@�5?@��^@�X@�Ĝ@��@�\)@���@��@��;@���@�S�@�
=@�n�@���@�&�@��@�j@�t�@��@��!@��@��!@���@�v�@���@�p�@���@��j@���@��D@�Z@�Z@� �@�1@���@�t�@�+@���@���@���@��y@���@���@�J@��7@�G�@�7L@��@��@�r�@�b@�b@�ƨ@��@��@��w@���@�;d@�@��R@�n�@��@���@���@��@���@��u@�Q�@��
@�dZ@��y@��R@�~�@�5?@���@���@�G�@�&�@��@�?}@�/@�%@��@�j@�9X@� �@�1@�1@���@��m@�t�@�"�@�
=@��y@��@�ȴ@���@�ff@�V@�M�@�=q@�-@��@���@���@��^@��h@��7@��7@�hs@�7L@��@��/@��j@��9@���@�Q�@�  @�1@�Z@�A�@�1@���@���@��@�S�@�K�@�;d@��@��R@�-@�@��-@���@��T@�?}@���@��`@��`@��/@��9@�r�@�Q�@�(�@��m@��@�dZ@�"�@��!@��+@�M�@�M�@�E�@�=q@�@��7@�O�@��@���@��/@���@�z�@�A�@�w@�@~{@}��@}�@|�D@|�@{��G�O�@w��@nE�@e�@\z�@S@J�!@D(�@=��@7��@1%@*�H@&5?@�w@9X@  @��@�@��@	�7@�+@=q111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B	  B	B	B	B	B	B	B	B	B	B	%B	DB	JB	�B	7LB	]/B	�qB	��B	�BB
{B
H�B
gmB
~�B
�JB
�%B
�oB
�B
�B
�
B
�B\B)�B2-B9XBH�BL�BR�BffB�DBK�B
��B
=B	7B$�Bp�B�B�dBɺB�BB/B�`B�B��B�ZB�TB�#B�qB�bB�1B�HB#�B\B�sB��B�VBz�BH�B
=B
�}B
� B
H�B
(�B
%B	�B	ŢB	�B	��B	�B	XB	6FB	0!B	'�B	uB	1B	B�B��B�^B�!B��B��B��B��B�\B�PB�JB�Bv�BjBgmB`BBbNBbNBgmBs�B�B�uB��B��B��B�{B�bB�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B�\B�\B��B��B�B�B�9B�9B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�JB�+B�B�B}�Bt�Bk�Be`B`BBdZBw�B|�B}�B~�B�B�B�B�B�B�B�1B�+B�+B�+B�1B�7B�=B�7B�B|�B~�B�+B�bB�{B��B��B��B��B�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�3B�dB�^B�?B�B�B�B�B�B�9B�XB�dB�qB��BÖBÖBÖBĜBĜBȴB��B��B��B��B�
B�/B�ZB�ZB�ZB�NB�`B�sB�B�B��B��B��B��B��B��B	  B	B	B	+B	
=B	bB	{B	�B	�B	�B	�B	�B	#�B	-B	2-B	49B	7LB	:^B	<jB	>wB	A�B	C�B	E�B	F�B	G�B	H�B	J�B	P�B	S�B	ZB	`BB	cTB	dZB	e`B	ffB	gmB	l�B	r�B	z�B	|�B	z�B	z�B	{�B	|�B	� B	�B	�%B	�1B	�1B	�7B	�7B	�=B	�DB	�VB	�hB	�bB	�VB	�bB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�-B	�?B	�LB	�RB	�RB	�XB	�dB	�qB	�wB	�wB	�wB	�wB	�wB	�}B	��B	��B	��B	��B	��B	��B	��B	B	ÖB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�#B	�5B	�;B	�;B	�;B	�;B	�5B	�5B	�5B	�5B	�;B	�;B	�NB	�ZB	�ZB	�`B	�`B	�`B	�fB	�mB	�mB	�mB	�mB	�mB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
%B
%B
+B
+B
+B
+B
1B
1B
+B
+B
+B
	7B
	7B
	7B
	7B
	7B

=B
	7B

=B
DB
DB
DB
JB
JB
PB
PB
PB
PB
VB
VB
\B
\B
�B
hB
�B
 �B
)�B
33B
9XB
@�B
E�B
L�B
Q�B
XB
\)B
aHB
dZB
iyB
n�B
p�B
s�B
w�B
z�B
~�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B��B��B��B��B��B��B� B��B	 	B	B	B	B	B	B	$B	B	B	&B	-B	NB	QB	�B	7QB	]0B	�rB	��B	�DB
xB
H�B
ghB
~�B
�BB
� B
�iB
�B
��B
�B
�BSB)�B2!B9LBH�BL�BR�BfYB�8BK�B
��B
4B	/B$�Bp�B��B�YBɯB�6B/B�TB��B˸B�NB�HB�B�gB�WB�'B�=B#�BPB�jB��B�KBz�BH�B
2B
�rB
�B
H�B
(�B
B	�B	ŠB	�
B	��B	�B	XB	6GB	0$B	'�B	xB	3B	B�B��B�fB�'B��B��B��B��B�fB�WB�RB�Bv�Bj�BgxB`JBbWBbWBgwBs�B�%B�|B��B��B��B��B�jB�vB��B��B��B��B��B��B��B��B��B��B�B��B��B��B�cB�bB��B�B�B�B�;B�=B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�PB�1B�B�B}�Bt�Bk�BegB`JBd^Bw�B|�B}�B B�B�B�B�B�B�"B�5B�0B�0B�0B�3B�<B�AB�<B�B|�B B�2B�gB�B��B��B��B��B��B�}B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�
B�B�6B�eB�^B�CB�B�B�B�B�B�8B�XB�eB�sB��BÙBÙBÚBĝBĝBȶB��B��B��B��B�	B�/B�[B�ZB�ZB�MB�^B�qB�B�B��B��B��B��B��B��B��B	B	B	)B	
;B	aB	xB	~B	�B	�B	�B	�B	#�B	-
B	2,B	44B	7IB	:ZB	<eB	>rB	A�B	C�B	E�B	F�B	G�B	H�B	J�B	P�B	S�B	ZB	`;B	cOB	dSB	eZB	f`B	geB	l�B	r�B	z�B	|�B	z�B	z�B	{�B	|�B	�B	�B	�B	�)B	�*B	�/B	�1B	�5B	�:B	�QB	�`B	�[B	�NB	�ZB	�|B	��B	��B	��B	��B	��B	��B	� B	�	B	�B	�B	�B	� B	�"B	�5B	�CB	�IB	�HB	�OB	�\B	�gB	�nB	�oB	�oB	�pB	�oB	�oB	�{B	�{B	�|B	�|B	��B	��B	��B	B	ÍB	ƝB	ȫB	ʺB	ʸB	˽B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�
B	�B	�B	�B	�*B	�0B	�1B	�0B	�3B	�*B	�+B	�+B	�+B	�0B	�0B	�BB	�QB	�OB	�XB	�TB	�UB	�]B	�cB	�cB	�eB	�dB	�dB	�hB	�pB	�qB	�qB	�uB	�uB	�uB	�vB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
!B
 B
"B
!B
'B
%B
$B
!B
B
	+B
	+B
	+B
	.B
	,B

4B
	-B

2B
9B
7B
7B
=B
?B
CB
EB
EB
FB
KB
KB
QB
QG�O�B
`B
�B
 �B
)�B
3)B
9MB
@wB
E�B
L�B
Q�B
XB
\B
a;B
dNB
imB
n�B
p�B
s�B
w�B
z�B
~�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.13 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436432016080714364320160807143643  AO  ARCAADJP                                                                    20160206101706    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160206101706  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160206101706  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143643  IP                  G�O�G�O�G�O�                