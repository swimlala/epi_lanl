CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T12:58:29Z creation      
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
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20181121125829  20190408133247  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5286                            2C  D   APEX                            6531                            072314                          846 @��<כ�W1   @��>`� @4�9XbN�b旍O�;1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @���@�  A   A   A@  A`  A~ffA�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D!��D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDys3D�  D�I�D���D��fD�fD�I�D���D��fD�	�D�,�D�s3D���D�fD�0 D�VfD��fD��3D�  D�` 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�@�(�A{A"{AB{Ab{A�=pA�
=A�
=A�
=A�
=A�
=A�
=A�
=B �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�B�B�B�B�B�B��)B��)B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C !HC!HC!HC!HC!HC
!HC!HC!HC!HC!HC!HC!HC!HC!HC!HC!HC !HC"!HC$!HC&!HC(!HC*!HC,!HC.!HC0!HC2!HC4!HC6!HC8!HC:!HC<!HC>!HC@!HCB!HCD!HCF!HCH!HCJ!HCL!HCN!HCP!HCR!HCT!HCV!HCX!HCZ!HC\!HC^!HC`!HCb!HCd!HCf!HCh!HCj!HCl!HCn!HCp!HCr!HCt!HCv!HCx!HCz!HC|!HC~!HC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D RD �RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD	RD	�RD
RD
�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD�D�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD RD �RD!RD!�RD"�D"�RD#RD#�RD$RD$�RD%RD%�RD&RD&�RD'RD'�RD(RD(�RD)RD)�RD*RD*�RD+RD+�RD,RD,�RD-RD-�RD.RD.�RD/RD/�RD0RD0�RD1RD1�RD2RD2�RD3RD3�RD4RD4�RD5RD5�RD6RD6�RD7RD7�RD8RD8�RD9RD9�RD:RD:�RD;RD;�RD<RD<�RD=RD=�RD>RD>�RD?RD?�RD@RD@�RDARDA�RDBRDB�RDCRDC�RDDRDD�RDERDE�RDFRDF�RDGRDG�RDHRDH�RDIRDI�RDJRDJ�RDKRDK�RDLRDL�RDMRDM�RDNRDN�RDORDO�RDPRDP�RDQRDQ�RDRRDR�RDSRDS�RDTRDT�RDURDU�RDVRDV�RDWRDW�RDXRDX�RDYRDY�RDZRDZ�RD[RD[�RD\RD\�RD]RD]�RD^RD^�RD_RD_�RD`RD`�RDaRDa�RDbRDb�RDcRDc�RDdRDd�RDeRDe�RDfRDf�RDgRDg�RDhRDh�RDiRDi�RDjRDj�RDkRDk�RDlRDl�RDmRDm�RDnRDn�RDoRDo�RDpRDp�RDqRDq�RDrRDr�RDsRDs�RDtRDt�RDtθDy{�D�)D�M�D���D�ʏD�
�D�M�D���D�ʏD��D�0�D�w\D���D�
�D�4)D�Z�D�ʏD��\D�$)D�d)11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�O�A�E�A�M�A�;dA�33A��A�VA��A��`A��`A�ȴA�ƨA�ĜA�ƨA�ĜA�ƨA�ƨA�ƨA�ȴA�ȴA�ƨA�ȴA���A���A��yA�  A�$�A�  A͕�A�;dA˓uA�XA��A�?}A�K�A��Aş�A�O�A¡�A�t�A���A�
=A���A�7LA�ȴA��yA�v�A��jA���A���A��A��A���A�^5A�oA���A�VA��A��FA�K�A���A�z�A��uA�K�A��7A���A�t�A�t�A�|�A��mA���A�(�A�XA�z�A�Q�A��A�{A�A�oA�7LA�XA�1A��A��A��A�hsA�ffA���A�l�A��A�+A��A���A���A�l�A���A�oA��A��`A�x�A��-A�M�A��mA�=qA�%A���A�O�A�n�A�;dA��TA�A�A��#A���A�ZA�7A|�`Ay�hAu�^Ar��Ao�Ao
=Ak�Ai33Af�jAdĜAa�TA`M�A^��A\-AX�jAT�AQ7LANv�AJn�AHr�AD�+AA�A@�DA>E�A<1A;x�A:��A:I�A8�RA6�/A4ĜA4 �A3�TA3l�A2��A0n�A.v�A-�hA,A�A+�A+t�A*�RA)�
A(�A(�\A'�TA'&�A&�9A&1'A%|�A$VA$bA#�mA#�A"�A"�9A"9XA!�mA!�-A!XA�TAdZA��A��AJA`BAoA�;A�A��A�A+AZA�-A�#A"�A��A�
A��A�A�A�jAO�@���@�?}@�z�@��u@�E�@��\@��^@�%@�bN@�@��^@��j@�\)@�%@�w@�^5@���@��T@�@�j@�+@�7@��@�;d@�1'@�9X@��@�+@��@���@�ƨ@�R@��@�|�@�-@�J@ݺ^@��@���@��`@��`@��`@ݩ�@�&�@�Z@�bN@ܬ@��@�Ĝ@۝�@�;d@��@�X@؃@׾w@�C�@�@պ^@�%@��
@��y@�p�@�+@��@�A�@�"�@���@�1@�"�@�V@ź^@���@�S�@��@��-@�x�@�&�@�%@���@�1'@�z�@�r�@�9X@�z�@��u@�1@��m@���@�+@�"�@���@��D@�?}@�V@���@�7L@��!@��@���@���@��-@�Ĝ@�\)@��H@��
@��@�?}@�%@��D@�33@���@�o@�dZ@��F@���@�l�@��@�v�@�^5@�5?@�@�x�@�%@��@�(�@�  @�  @��;@��@�K�@�
=@��!@�~�@�^5@�{@���@��@���@��`@�Ĝ@��9@��@�Z@���@���@�l�@���@�@�O�@���@��@��/@��j@�I�@�1'@�  @��P@�o@��H@��+@�$�@��T@�`B@�/@��`@��@��@�  @���@�\)@���@��!@��R@���@�M�@�$�@�{@��@�@��h@�p�@�%@���@�Q�@�b@��m@�C�@�v�@�$�@�$�@�V@���@���@��P@�@��\@�ff@�J@�(�@�dZ@��@���@�{@��T@�@��^@�x�@���@���@��u@�Ĝ@�bN@�I�@�A�@�1@�ƨ@��P@�\)@�33@�@��H@���@�~�@�v�@�V@�@���@�&�@��@�  @��@��@�S�@�\)@�;d@�@�p�@��+@�S�@�|�@�\)@�
=@��H@��H@�~�@�^5@�E�@�$�@�J@��@���@��@�/@��@��9@�z�@��@��m@���@�l�@���@�n�@�n�@�^5@�E�@��@�@�`B@�&�@�&�@��@�I�@��@��;@��@���@�dZ@�"�@�S�@�;d@�"�@��!@���@��\@�=q@�-@���@��^@�/@��/@�1@�v�@�5?@y�7@r^5@i�#@a&�@Wl�@M�@G��@@Q�@:~�@5�h@/l�@*=q@#dZ@\)@��@�+@o@|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   A�O�A�E�A�M�A�;dA�33A��A�VA��A��`A��`A�ȴA�ƨA�ĜA�ƨA�ĜA�ƨA�ƨA�ƨA�ȴA�ȴA�ƨA�ȴA���A���A��yA�  A�$�A�  A͕�A�;dA˓uA�XA��A�?}A�K�A��Aş�A�O�A¡�A�t�A���A�
=A���A�7LA�ȴA��yA�v�A��jA���A���A��A��A���A�^5A�oA���A�VA��A��FA�K�A���A�z�A��uA�K�A��7A���A�t�A�t�A�|�A��mA���A�(�A�XA�z�A�Q�A��A�{A�A�oA�7LA�XA�1A��A��A��A�hsA�ffA���A�l�A��A�+A��A���A���A�l�A���A�oA��A��`A�x�A��-A�M�A��mA�=qA�%A���A�O�A�n�A�;dA��TA�A�A��#A���A�ZA�7A|�`Ay�hAu�^Ar��Ao�Ao
=Ak�Ai33Af�jAdĜAa�TA`M�A^��A\-AX�jAT�AQ7LANv�AJn�AHr�AD�+AA�A@�DA>E�A<1A;x�A:��A:I�A8�RA6�/A4ĜA4 �A3�TA3l�A2��A0n�A.v�A-�hA,A�A+�A+t�A*�RA)�
A(�A(�\A'�TA'&�A&�9A&1'A%|�A$VA$bA#�mA#�A"�A"�9A"9XA!�mA!�-A!XA�TAdZA��A��AJA`BAoA�;A�A��A�A+AZA�-A�#A"�A��A�
A��A�A�A�jAO�@���@�?}@�z�@��u@�E�@��\@��^@�%@�bN@�@��^@��j@�\)@�%@�w@�^5@���@��T@�@�j@�+@�7@��@�;d@�1'@�9X@��@�+@��@���@�ƨ@�R@��@�|�@�-@�J@ݺ^@��@���@��`@��`@��`@ݩ�@�&�@�Z@�bN@ܬ@��@�Ĝ@۝�@�;d@��@�X@؃@׾w@�C�@�@պ^@�%@��
@��y@�p�@�+@��@�A�@�"�@���@�1@�"�@�V@ź^@���@�S�@��@��-@�x�@�&�@�%@���@�1'@�z�@�r�@�9X@�z�@��u@�1@��m@���@�+@�"�@���@��D@�?}@�V@���@�7L@��!@��@���@���@��-@�Ĝ@�\)@��H@��
@��@�?}@�%@��D@�33@���@�o@�dZ@��F@���@�l�@��@�v�@�^5@�5?@�@�x�@�%@��@�(�@�  @�  @��;@��@�K�@�
=@��!@�~�@�^5@�{@���@��@���@��`@�Ĝ@��9@��@�Z@���@���@�l�@���@�@�O�@���@��@��/@��j@�I�@�1'@�  @��P@�o@��H@��+@�$�@��T@�`B@�/@��`@��@��@�  @���@�\)@���@��!@��R@���@�M�@�$�@�{@��@�@��h@�p�@�%@���@�Q�@�b@��m@�C�@�v�@�$�@�$�@�V@���@���@��P@�@��\@�ff@�J@�(�@�dZ@��@���@�{@��T@�@��^@�x�@���@���@��u@�Ĝ@�bN@�I�@�A�@�1@�ƨ@��P@�\)@�33@�@��H@���@�~�@�v�@�V@�@���@�&�@��@�  @��@��@�S�@�\)@�;d@�@�p�@��+@�S�@�|�@�\)@�
=@��H@��H@�~�@�^5@�E�@�$�@�J@��@���@��@�/@��@��9@�z�@��@��m@���@�l�@���@�n�@�n�@�^5@�E�@��@�@�`B@�&�@�&�@��@�I�@��@��;@��@���@�dZ@�"�@�S�@�;d@�"�@��!@���@��\@�=q@�-@���@��^@�/@��/G�O�@�v�@�5?@y�7@r^5@i�#@a&�@Wl�@M�@G��@@Q�@:~�@5�h@/l�@*=q@#dZ@\)@��@�+@o@|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
��B
��B
��B
��B
��B
��B
��B
��B
ɺB
ɺB
ɺB
ɺB
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
�#B
�ZBDB5?B>wB@�BE�BF�BI�B_;Br�B�{B��B��BB��B#�B�BF�BM�BN�BS�B[#B^5Br�Bs�Bq�By�Bx�By�Bz�B�B�{B��B��B��B��B��B��B��B�hB�{B��B��B��B��B��B��B�JB�1B�DB�+B|�Bl�BdZB\)BT�BO�BL�BD�B33B(�B�B��B�B�B�5B��BÖB��B�=Bx�Bk�B`BBW
B:^BbB
�B
�B
ŢB
�B
�uB
�1B
z�B
jB
T�B
5?B
�B	�B	�B	��B	�LB	��B	~�B	hsB	S�B	S�B	;dB	'�B	�B	+B��B��B�B�NB��B�XB��B�{B{�Bp�B^5B`BBn�Bs�Bz�B�DB�=B�=B�1B�+B�1B�B�B� B{�Bs�Bk�BffBaHBaHBp�Bw�Bw�Bw�Bw�By�B}�B~�B�B�JB�VB�bB��B��B��B�?B�9B�3B�3B�'B��B��B�1Bz�Bp�Bk�B`BBXBT�BO�BK�BS�BYBW
BW
BT�BP�BL�BH�BF�B@�B5?B-B'�B%�B&�B.BN�B^5BgmBm�Bp�Bn�Bl�Bl�Bm�Bl�BjBhsBffBe`BdZBdZBdZBdZBcTBe`Br�Bz�By�B}�B}�B~�B}�B}�B}�B�B�B�B�B�+B�DB�\B�hB��B��B�B�B�3B�dB��BBBÖBÖBȴB��B��B��B��B��B��B��B��B��B��B��BÖB�wB�RB�?B�-B�'B�3B�-B�!B�-B�9B�?B�XB�jB�}BȴB��B�B�#B�;B�HB�NB�yB�yB�yB�B�B��B��B	B��B��B	
=B	\B	VB	VB	\B	bB	\B	uB	 �B	!�B	 �B	 �B	�B	�B	 �B	%�B	)�B	33B	8RB	:^B	=qB	?}B	?}B	?}B	@�B	A�B	B�B	E�B	J�B	M�B	N�B	P�B	VB	XB	YB	]/B	_;B	_;B	bNB	cTB	cTB	e`B	k�B	l�B	l�B	l�B	o�B	q�B	s�B	t�B	x�B	|�B	�B	�B	�B	�B	�B	�+B	�1B	�=B	�JB	�VB	�\B	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�-B	�3B	�9B	�3B	�-B	�3B	�9B	�-B	�!B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�3B	�9B	�FB	�LB	�LB	�RB	�XB	�XB	�XB	�RB	�FB	�?B	�9B	�9B	�?B	�?B	�3B	�?B	�wB	ÖB	ŢB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�#B	�)B	�BB	�HB	�NB	�TB	�ZB	�ZB	�`B	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B
DB	��B
+B
bB
�B
 �B
(�B
1'B
9XB
=qB
B�B
G�B
L�B
S�B
XB
_;B
aHB
ffB
k�B
m�B
q�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   B
ʷB
ʸB
˻B
ʴB
ʵB
ʶB
ʹB
ʵB
ɰB
ɰB
ɯB
ɯB
ʶB
ʶB
ʷB
˿B
˿B
��B
��B
��B
��B
��B
��B
��B
�B
�NB9B54B>lB@zBE�BF�BI�B_2Br�B�pB��B��BB��B#�B�BF�BM�BN�BS�B[B^*Br�Bs�Bq�By�Bx�By�Bz�B�B�oB��B�{B��B��B��B��B�uB�[B�lB��B��B��B��B��B��B�=B�%B�5B�!B|�Bl{BdMB\ BT�BO�BL�BD�B3(B(�B�B��B�B�zB�)B��BÈB��B�/Bx�BkxB`4BW B:QBVB
�B
��B
ŗB
�	B
�hB
�'B
z�B
jrB
T�B
52B
B	�B	�B	��B	�@B	��B	~�B	heB	S�B	S�B	;WB	'�B	�B	B��B��B�xB�?B��B�IB��B�nB{�Bp�B^)B`4Bn�Bs�Bz�B�6B�.B�.B�!B�B� B�B�B�B{�Bs�BkuBfUBa:Ba:Bp�Bw�Bw�Bw�Bw�By�B}�B~�B�	B�;B�FB�SB��B��B��B�.B�+B�$B�#B�B��B�}B�#Bz�Bp�BkvB`5BXBT�BO�BK�BS�BY	BV�BV�BT�BP�BL�BH�BF�B@rB50B,�B'�B%�B&�B.BN�B^%Bg\Bm�Bp�Bn�BlzBl{Bm�Bl{BjoBheBfZBePBdHBdKBdJBdIBcFBeNBr�Bz�By�B}�B}�B~�B}�B}�B}�B��B�B�B�B�B�4B�NB�WB��B��B��B�B�"B�TB�yBB�BÅBÇBȤB��B˸BʯB̼B��B��B��B��B̽B̻BʱBÆB�cB�@B�-B�B�B�$B�B�B�B�(B�.B�HB�ZB�nBȤB��B�B�B�+B�7B�=B�hB�lB�hB�pB�B��B��B	 �B��B��B	
-B	JB	FB	FB	LB	TB	NB	aB	 �B	!�B	 �B	 �B	�B	�B	 �B	%�B	)�B	3!B	8AB	:MB	=aB	?kB	?lB	?lB	@tB	AwB	BB	E�B	J�B	M�B	N�B	P�B	U�B	W�B	YB	]B	_)B	_*B	b=B	cEB	cCB	eQB	ksB	l{B	l|B	l{B	o�B	q�B	s�B	t�B	x�B	|�B	��B	� B	�B	�B	�	B	�B	�!B	�*B	�9B	�EB	�KB	�WB	�eB	�jB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�#B	�'B	�!B	�B	�"B	�%B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	� B	�)B	�4B	�;B	�;B	�@B	�KB	�DB	�IB	�?B	�5B	�+B	�(B	�'B	�-B	�.B	�"B	�+B	�gB	ÅB	őB	œB	ǜB	ʰB	ʰB	̼B	˶B	̽B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�/B	�7B	�<B	�AB	�HB	�JB	�OB	�]B	�dB	�aB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��G�O�B	��B
B
RB
yB
 �B
(�B
1B
9GB
=^B
B}B
G�B
L�B
S�B
X B
_)B
a8B
fVB
ksB
mB
q�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.13 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904081332472019040813324720190408133247  AO  ARCAADJP                                                                    20181121125829    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121125829  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121125829  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190408133247  IP                  G�O�G�O�G�O�                