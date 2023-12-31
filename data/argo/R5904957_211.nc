CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:44Z creation      
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
resolution        =���   axis      Z          9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       BD   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  IT   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       K   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       R(   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Y8   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       Z�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  b   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       c�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       j�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  q�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       s�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  z�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       |�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20181024140844  20181024140844  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @��dȠ��1   @��effy@5#S����d�hr�!1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   B   @�33@�  @���A   A@  A`  A�  A�33A�  A�  A�  A�  A�  A�  B   BffB  B  B��B'��B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B���B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CC�fCF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CY�fC\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�fC�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C��C�  C��3C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  D   D � D  D� D  Dy�D  D� D  D�fD  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD�fD  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� DfD� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-fD-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3�fD4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DBy�DB��DCy�DD  DDy�DE  DE� DFfDF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV�fDW  DW� DX  DX� DY  DY� DZ  DZ�fD[  D[�fD\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da�fDb  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� DjfDj�fDkfDk� Dl  Dl� Dl��Dm� Dn  Dn�fDofDo� Dp  Dp� Dq  Dqy�Dq��Dr� DsfDs�fDt  Dt� Du  Du� Dv  Dv� Dv��Dwy�Dw� Dy��D�FD�d�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�\)@�(�A z�A"{AB{Ab{A�
=A�=pA�
=A�
=A�
=A�
=A�
=A�
=B �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�B�B�\B�B�B�B�B�B�B�u�B�u�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C!HC!HC !HC"!HC$!HC&!HC(!HC*!HC,!HC.!HC0!HC2!HC4!HC6!HC8!HC:!HC<!HC>!HC@!HCB!HCD�CF!HCH!HCJ!HCL!HCN!HCP!HCR!HCT!HCV!HCX!HCZ�C\!HC^!HC`!HCb!HCd!HCf!HCh!HCj!HCl!HCn!HCp!HCr!HCt!HCv!HCx!HCz!HC|!HC~!HC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�qC�qC��C��C��C�qC��C��C�qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�qC�qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D RD �RDRD�RDRD��DRD�RDRD��DRD�RDRD�RDRD�RDRD�RD	RD	�RD
RD
�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD�D��DRD��DRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD�D�RDRD�RDRD�RDRD�RDRD�RD�D�RD RD �RD!RD!�RD"RD"�RD#RD#�RD$RD$�RD%RD%�RD&RD&�RD'RD'�RD(RD(�RD)RD)�RD*RD*�RD+RD+�RD,RD,�RD-�D-�RD.RD.�RD/RD/�RD0RD0�RD1RD1�RD2RD2�RD3RD3��D4RD4�RD5RD5�RD6RD6�RD7RD7�RD8RD8�RD9RD9�RD:RD:�RD;RD;�RD<RD<�RD=RD=�RD>RD>�RD?RD?�RD@RD@�RDARDA�RDBRDB��DC�DC��DDRDD��DERDE�RDF�DF�RDGRDG�RDHRDH�RDIRDI�RDJRDJ�RDKRDK�RDLRDL�RDMRDM�RDNRDN�RDORDO�RDPRDP�RDQRDQ�RDRRDR�RDSRDS�RDTRDT�RDURDU�RDVRDV��DWRDW�RDXRDX�RDYRDY�RDZRDZ��D[RD[��D\RD\�RD]RD]�RD^RD^�RD_RD_�RD`RD`�RDaRDa��DbRDb�RDcRDc�RDdRDd�RDeRDe�RDfRDf�RDgRDg�RDhRDh�RDiRDi�RDj�Dj��Dk�Dk�RDlRDl�RDm�Dm�RDnRDn��Do�Do�RDpRDp�RDqRDq��Dr�Dr�RDs�Ds��DtRDt�RDuRDu�RDvRDv�RDw�Dw��Dw�RDy�3D�J=D�h�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�\)A�^5A�dZA�p�A�p�A�r�A�p�A�p�A�l�A�jA�`BA�bNA�K�A�I�A�;dA�K�A�1A�"�A�
=A��TA�ȴAƋDA�z�A�|�A�|�A�~�A�|�A�~�AƁAƁA�|�A�z�A��;A�&�Aĕ�A�x�A�A�AþwAÅA�bNA�1'A��HA�O�A�I�A���A�ZA�1'A�l�A�hsA���A�ƨA��uA�9XA�~�A� �A�K�A�  A���A�M�A��mA�l�A���A�7LA�=qA�~�A��A�M�A��yA�hsA���A���A���A��^A��7A��uA�?}A��DA�XA�1'A|VAz~�Ax�jAv��Au��Au
=Atr�As��AsS�Ar��Ap�ApAn$�Amt�Aj�Ah�Ag��Ae;dAbA_�
A^n�A]�A[��AX9XAVffAT�DAQ��AO��AL�yAKƨAKoAJQ�AIXAE�#AD��ACx�A@�uA?/A>�RA>VA=�;A=\)A<9XA<$�A;G�A9�PA7��A6�RA5K�A4=qA2��A1�wA1+A0n�A/p�A-�
A,-A+S�A+oA*��A)��A'&�A$E�A#��A#�PA#dZA!�^A��A�mAv�Az�AffAZA�
A��A�hA�7Ax�A�A�RA��At�A��A~�A{Al�A5?A��A
�`A
jA	�#A	VA1'A��A�AVA�A�yA�yAn�A�^A��A �!@�%@��@�@���@��`@�5?@�^5@���@��/@��;@�\)@��@�M�@�%@ץ�@���@��@Դ9@Ь@��@�l�@��@��@���@�~�@���@�@�@͉7@�G�@�%@���@̼j@̬@�z�@��
@˕�@�t�@�+@�ff@�%@�\)@�n�@�x�@�&�@���@��;@öF@�;d@���@�ȴ@°!@�@�hs@��D@�bN@��F@�\)@�33@�"�@�o@��@�n�@�@��`@�o@�7L@���@���@�5?@���@��T@��@��@��!@�x�@�z�@���@�^5@��u@�bN@�
=@���@��7@��7@��@��
@�hs@��#@�@�X@���@���@��9@�9X@���@�33@�
=@�v�@���@�x�@�X@�?}@���@���@��@�l�@�C�@�33@�
=@���@�ff@��7@�&�@��@���@��@�j@�bN@�bN@�A�@��;@���@��@��@��P@��@�\)@�+@��@�V@���@���@���@���@��^@��7@�X@�V@�Q�@��@�  @��w@�K�@��@��H@���@��+@�M�@���@�G�@��@�A�@�ƨ@�@���@��@��^@��-@��@�p�@�/@��j@��@�r�@�bN@�A�@���@���@��@�t�@�l�@�dZ@�+@�
=@��!@���@���@�=q@��-@���@���@���@���@�V@���@��@�x�@�hs@�%@�j@�b@�  @��m@��P@�\)@�33@�o@��y@���@�ff@�{@���@��-@���@��@�O�@�/@��@�Ĝ@�j@�A�@�1'@��@���@�K�@�o@���@��H@���@�v�@�E�@�{@��#@���@���@�@��-@�p�@�?}@�V@��D@���@��D@�Z@�r�@�I�@�9X@�  @��m@��w@��w@��w@�ƨ@�ƨ@���@��w@��P@�\)@��@��y@�ȴ@��\@�5?@���@��@��T@���@���@���@���@��@��@��@�Ĝ@�Z@�Q�@�=q@u?}@e\�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�\)A�^5A�dZA�p�A�p�A�r�A�p�A�p�A�l�A�jA�`BA�bNA�K�A�I�A�;dA�K�A�1A�"�A�
=A��TA�ȴAƋDA�z�A�|�A�|�A�~�A�|�A�~�AƁAƁA�|�A�z�A��;A�&�Aĕ�A�x�A�A�AþwAÅA�bNA�1'A��HA�O�A�I�A���A�ZA�1'A�l�A�hsA���A�ƨA��uA�9XA�~�A� �A�K�A�  A���A�M�A��mA�l�A���A�7LA�=qA�~�A��A�M�A��yA�hsA���A���A���A��^A��7A��uA�?}A��DA�XA�1'A|VAz~�Ax�jAv��Au��Au
=Atr�As��AsS�Ar��Ap�ApAn$�Amt�Aj�Ah�Ag��Ae;dAbA_�
A^n�A]�A[��AX9XAVffAT�DAQ��AO��AL�yAKƨAKoAJQ�AIXAE�#AD��ACx�A@�uA?/A>�RA>VA=�;A=\)A<9XA<$�A;G�A9�PA7��A6�RA5K�A4=qA2��A1�wA1+A0n�A/p�A-�
A,-A+S�A+oA*��A)��A'&�A$E�A#��A#�PA#dZA!�^A��A�mAv�Az�AffAZA�
A��A�hA�7Ax�A�A�RA��At�A��A~�A{Al�A5?A��A
�`A
jA	�#A	VA1'A��A�AVA�A�yA�yAn�A�^A��A �!@�%@��@�@���@��`@�5?@�^5@���@��/@��;@�\)@��@�M�@�%@ץ�@���@��@Դ9@Ь@��@�l�@��@��@���@�~�@���@�@�@͉7@�G�@�%@���@̼j@̬@�z�@��
@˕�@�t�@�+@�ff@�%@�\)@�n�@�x�@�&�@���@��;@öF@�;d@���@�ȴ@°!@�@�hs@��D@�bN@��F@�\)@�33@�"�@�o@��@�n�@�@��`@�o@�7L@���@���@�5?@���@��T@��@��@��!@�x�@�z�@���@�^5@��u@�bN@�
=@���@��7@��7@��@��
@�hs@��#@�@�X@���@���@��9@�9X@���@�33@�
=@�v�@���@�x�@�X@�?}@���@���@��@�l�@�C�@�33@�
=@���@�ff@��7@�&�@��@���@��@�j@�bN@�bN@�A�@��;@���@��@��@��P@��@�\)@�+@��@�V@���@���@���@���@��^@��7@�X@�V@�Q�@��@�  @��w@�K�@��@��H@���@��+@�M�@���@�G�@��@�A�@�ƨ@�@���@��@��^@��-@��@�p�@�/@��j@��@�r�@�bN@�A�@���@���@��@�t�@�l�@�dZ@�+@�
=@��!@���@���@�=q@��-@���@���@���@���@�V@���@��@�x�@�hs@�%@�j@�b@�  @��m@��P@�\)@�33@�o@��y@���@�ff@�{@���@��-@���@��@�O�@�/@��@�Ĝ@�j@�A�@�1'@��@���@�K�@�o@���@��H@���@�v�@�E�@�{@��#@���@���@�@��-@�p�@�?}@�V@��D@���@��D@�Z@�r�@�I�@�9X@�  @��m@��w@��w@��w@�ƨ@�ƨ@���@��w@��P@�\)@��@��y@�ȴ@��\@�5?@���@��@��T@���@���@���@���@��@��@��@�Ĝ@�Z@�Q�@�=q@u?}@e\�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B!�B!�B!�B!�B!�B!�B!�B!�B!�B"�B#�B$�B&�B&�B(�B&�B.B+B-B0!B33B9XB;dB;dB<jB<jB;dB;dB=qB>wBB�BD�B\)Bm�By�B�B�oB�LB�qB��BĜBŢBŢB�wB�RB�?B�3B�B�3B�LB�qB��?�I�B�RB�3B��B��B�uB�=B�%B}�B[#BD�B33B(�B1B
��B
��B
�B
�#B
��B
ȴB
ȴB
ǮB
�^B
�FB
�B
�oB
l�B
L�B
@�B
5?B
&�B
�B
�B
�B
uB
\B
PB

=B
1B
B	��B	�HB	��B	B	�'B	��B	�7B	y�B	r�B	bNB	A�B	-B	 �B	�B	\B	DB	%B	B	B��B�B�B�`B�/B�B�
B��B��B��B��B��BȴBĜB��B�qB�XB�?B�-B�B�B�B��B��B��B��B��B��B��B��B��B�oB�hB�\B�PB�DB�=B�1B}�BgmB\)B[#B\)B\)B\)B[#B[#BZBZBZB[#B\)B]/B\)B[#BZB\)B]/BaHBl�Bn�Bk�Bm�Bo�Bp�Bq�Bp�Bp�Bl�BdZBW
BH�B@�B?}B@�BB�Bw�B�B�+B�=B�PB�VB�VB�bB�uB�oB�uB�oB�bB�PB�DB�=B�JB�\B�oB��B��B�^B�jB�wB�}B��B��B��B��BBŢBƨBƨBƨBȴBǮBƨBǮBɺB��B��B��B�B�/B�BB�BB�BB�`B�sB�B�yB�B�B�B�B�B�B��B��B��B��B	B	+B	+B	B	  B��B��B�B�B�mB�`B�TB�TB�B��B	DB	�B	�B	 �B	%�B	2-B	>wB	D�B	F�B	L�B	P�B	S�B	W
B	W
B	W
B	VB	T�B	T�B	VB	W
B	XB	XB	XB	\)B	^5B	`BB	`BB	aHB	aHB	cTB	dZB	iyB	k�B	o�B	p�B	q�B	r�B	t�B	w�B	y�B	|�B	}�B	~�B	~�B	� B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�7B	�=B	�=B	�JB	�oB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�3B	�?B	�FB	�XB	�^B	�^B	�dB	�jB	�jB	�qB	�qB	�qB	�dB	�^B	�dB	�dB	�dB	�dB	�dB	�jB	�jB	�qB	�}B	��B	��B	B	ĜB	ŢB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�#B	�)B	�/B	�5B	�;B	�;B	�;B	�BB	�HB	�HB	�HB	�TB	�fB	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B	��B	�cB
�B
,�11111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B!�B!�B!�B!�B!�B!�B!�B!�B!�B"�B#�B$�B&�B&�B(�B&�B.B+B-B0!B33B9XB;dB;dB<jB<jB;dB;dB=qB>wBB�BD�B\)Bm�By�B�B�oB�LB�qB��BĜBŢBŢB�wB�RB�?B�3B�B�3B�LB�qB��?�I�B�RB�3B��B��B�uB�=B�%B}�B[#BD�B33B(�B1B
��B
��B
�B
�#B
��B
ȴB
ȴB
ǮB
�^B
�FB
�B
�oB
l�B
L�B
@�B
5?B
&�B
�B
�B
�B
uB
\B
PB

=B
1B
B	��B	�HB	��B	B	�'B	��B	�7B	y�B	r�B	bNB	A�B	-B	 �B	�B	\B	DB	%B	B	B��B�B�B�`B�/B�B�
B��B��B��B��B��BȴBĜB��B�qB�XB�?B�-B�B�B�B��B��B��B��B��B��B��B��B��B�oB�hB�\B�PB�DB�=B�1B}�BgmB\)B[#B\)B\)B\)B[#B[#BZBZBZB[#B\)B]/B\)B[#BZB\)B]/BaHBl�Bn�Bk�Bm�Bo�Bp�Bq�Bp�Bp�Bl�BdZBW
BH�B@�B?}B@�BB�Bw�B�B�+B�=B�PB�VB�VB�bB�uB�oB�uB�oB�bB�PB�DB�=B�JB�\B�oB��B��B�^B�jB�wB�}B��B��B��B��BBŢBƨBƨBƨBȴBǮBƨBǮBɺB��B��B��B�B�/B�BB�BB�BB�`B�sB�B�yB�B�B�B�B�B�B��B��B��B��B	B	+B	+B	B	  B��B��B�B�B�mB�`B�TB�TB�B��B	DB	�B	�B	 �B	%�B	2-B	>wB	D�B	F�B	L�B	P�B	S�B	W
B	W
B	W
B	VB	T�B	T�B	VB	W
B	XB	XB	XB	\)B	^5B	`BB	`BB	aHB	aHB	cTB	dZB	iyB	k�B	o�B	p�B	q�B	r�B	t�B	w�B	y�B	|�B	}�B	~�B	~�B	� B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�7B	�=B	�=B	�JB	�oB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�3B	�?B	�FB	�XB	�^B	�^B	�dB	�jB	�jB	�qB	�qB	�qB	�dB	�^B	�dB	�dB	�dB	�dB	�dB	�jB	�jB	�qB	�}B	��B	��B	B	ĜB	ŢB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�#B	�)B	�/B	�5B	�;B	�;B	�;B	�BB	�HB	�HB	�HB	�TB	�fB	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B	��B	�cB
�B
,�11111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.13 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140844                              AO  ARCAADJP                                                                    20181024140844    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140844  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140844  QCF$                G�O�G�O�G�O�0               