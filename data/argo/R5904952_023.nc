CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:10Z creation      
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
_FillValue                 �  @,   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  A�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  JH   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Q   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  W�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Yp   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  `,   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  a�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  oT   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  yp   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �,   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �\   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �\   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �\   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �\   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �    HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005190510  20181005190510  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @װ��y�'1   @װ�:�B@2��/���c~-V1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�33A�33A�  A�  B   B  B  BffB ffB(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bw��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0�C2  C4  C6�C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C}�fC�fC��3C�  C��C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C�  C�  C��C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C��C��D   D y�D  D� DfD� D  D� D  D� D  D�fD  Dy�D  D� D��D� D	  D	� D
  D
� D  D� D  Dy�D  D� D  D� DfD�fDfD� D��D� D  D�fDfD� DfD� D  D�fD��D� D  D� D��Dy�D  D�fDfD�fD  D� D  D� D  D�fDfD�fDfD�fD fD �fD!  D!y�D"  D"y�D"��D#y�D#��D$y�D%  D%� D&  D&� D'fD'�fD(  D(y�D(��D)� D*  D*y�D*��D+� D,  D,� D-  D-� D.  D.� D/fD/�fD0  D0� D0��D1� D1��D2y�D2��D3� D4  D4� D5  D5� D6  D6�fD7  D7�fD8fD8� D8��D9� D:  D:� D;  D;� D<  D<y�D=  D=�fD>  D>� D?  D?� D@fD@�fDA  DA� DBfDB� DB��DC� DD  DD� DEfDE� DE��DF� DG  DG�fDHfDH� DIfDI� DJ  DJ� DK  DK� DL  DL� DM  DM� DNfDN�fDO  DOy�DP  DP� DQ  DQ� DR  DR� DR��DS� DTfDT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZy�D[  D[�fD\  D\� D\��D]y�D]��D^y�D_  D_� D`  D`� Da  Da� Db  DyqHD�K�D��R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��\@�(�A{A"{AB{Ab{A�
=A�
=A�
=A�
=A�=pA�=pA�
=A�
=B �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�u�B�u�B�u�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C !HC!HC!HC!HC!HC
!HC!HC!HC!HC!HC!HC!HC!HC!HC!HC!HC !HC"!HC$!HC&!HC(!HC*!HC,!HC.!HC0:�C2!HC4!HC6:�C8!HC:!HC<!HC>!HC@!HCB!HCD!HCF!HCH!HCJ!HCL!HCN!HCP!HCR!HCT!HCV!HCX!HCZ!HC\!HC^!HC`!HCb!HCd!HCf!HCh!HCj!HCl!HCn!HCp!HCr!HCt!HCv!HCx!HCz!HC|!HC~�C��C��C��C�qC�qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�qC�qC��C��C��C��C��C��C��C��C�qC��C��C��C��C��C��C��C��C��C�qC�qC��C��C��C��C�qC��C��C��C��C��C��C��C�qC��C��C��C��C��C��C��C�qC�qC��C��C��C��C��C��C��C��C��C��C�qC��C��C��C�qC�qC�qD RD ��DRD�RD�D�RDRD�RDRD�RDRD��DRD��DRD�RD�D�RD	RD	�RD
RD
�RDRD�RDRD��DRD�RDRD�RD�D��D�D�RD�D�RDRD��D�D�RD�D�RDRD��D�D�RDRD�RD�D��DRD��D�D��DRD�RDRD�RDRD��D�D��D�D��D �D ��D!RD!��D"RD"��D#�D#��D$�D$��D%RD%�RD&RD&�RD'�D'��D(RD(��D)�D)�RD*RD*��D+�D+�RD,RD,�RD-RD-�RD.RD.�RD/�D/��D0RD0�RD1�D1�RD2�D2��D3�D3�RD4RD4�RD5RD5�RD6RD6��D7RD7��D8�D8�RD9�D9�RD:RD:�RD;RD;�RD<RD<��D=RD=��D>RD>�RD?RD?�RD@�D@��DARDA�RDB�DB�RDC�DC�RDDRDD�RDE�DE�RDF�DF�RDGRDG��DH�DH�RDI�DI�RDJRDJ�RDKRDK�RDLRDL�RDMRDM�RDN�DN��DORDO��DPRDP�RDQRDQ�RDRRDR�RDS�DS�RDT�DT�RDURDU�RDVRDV�RDWRDW�RDXRDX�RDYRDY�RDZRDZ��D[RD[��D\RD\�RD]�D]��D^�D^��D_RD_�RD`RD`�RDaRDa�RDbRDyy�D�O�D��{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�33A�9XA�A�A�5?A�1'A�33A�5?A�7LA��A���A�JA�1A��mA��`A��mA��TAУ�A�l�A�33A���Aϝ�Aϕ�Aω7A�%A�|�A�A�A��A���A��#Aͥ�A�~�A�A�A�VA̾wAˡ�A��A��TAʰ!Aʏ\A�M�A�7LA�/A�/AɮA��A�I�A��HA�l�A��TA�x�A���A���A�bNAĉ7A�C�A�1'A��`A�-A���A���A��A�?}A�A��!A��PA�x�A�E�A���A���A�r�A�E�A� �A�r�A���A���A��A��A�`BA�(�A�A��PA���A�jA��A�E�A��PA�/A�G�A� �A��;A� �A��A�"�A�1A�O�A��;A�^5A��hA���A���A�
=A�x�A�A�A�ƨA�/A��jA��A�A�33A�  A"�A~��A~bNA|�Ay��AvbAr�/Aq�ApM�AhbNAct�Ab�\AbE�Aa��A`�A^��A]�#AZ��AX��AX�AXbNAX�AV�9AT9XAR��AM�mAI?}AGVACdZABJAA��AA�A@  A=�A;%A:1'A7�A5XA4�!A3�;A2�A1ƨA0^5A.�jA.�A,��A+�#A*�/A*�A)VA't�A%�FA"��A�hA�A��A�A��A�9AdZAS�A��A7LAdZA�`AbNA��A+A��AA�A|�A�!AZAƨA5?A
��A
�+A
  A��A-A1A�;A�
A�wAdZA�+A��A-AS�A�A�@���@��@�bN@�Z@�Q�@���@���@�n�@�X@�K�@��`@�Q�@ם�@��#@���@���@ӍP@�dZ@�C�@���@�$�@љ�@���@�K�@���@��;@ˮ@�t�@�K�@�;d@�@ʏ\@ȓu@�ff@��@�&�@�x�@�?}@��/@��@�ȴ@���@�`B@��`@���@���@�Z@�(�@�  @��m@��
@�|�@��R@�n�@�E�@�@���@�7L@��D@�r�@�I�@�(�@�o@�{@�J@��@�@���@��@�$�@�5?@��h@�/@�V@��-@��@��@��@���@�ff@�+@��@�I�@��u@��T@��#@�%@�"�@�1@���@�dZ@�C�@�;d@���@��m@�1'@�9X@� �@��@��9@��9@��9@���@�I�@���@�;d@�@��R@���@�v�@�=q@��@��#@���@���@��9@�bN@��@���@��P@�l�@�o@���@�J@��@�G�@�7L@��@���@��u@� �@�  @���@�"�@�v�@�M�@��@��@���@�p�@�G�@�j@���@��;@�  @�l�@���@�G�@��@�  @�  @��m@�ƨ@��P@�dZ@��@��w@��w@��F@�l�@��@��-@��@��u@���@�Q�@�j@� �@�n�@��`@�b@���@�z�@�Q�@�b@���@�1@�r�@�%@�`B@�x�@�/@��@�Ĝ@�Z@�b@��@��@�l�@�C�@�;d@�33@���@�r�@��w@�ƨ@�ȴ@�M�@�-@��-@��-@���@�9X@���@���@��T@��`@�Z@��@��@�S�@��@��+@�^5@�5?@�{@�J@�@�J@�J@��@��T@��#@��h@�7L@�V@�%@���@�_�@l�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�33A�9XA�A�A�5?A�1'A�33A�5?A�7LA��A���A�JA�1A��mA��`A��mA��TAУ�A�l�A�33A���Aϝ�Aϕ�Aω7A�%A�|�A�A�A��A���A��#Aͥ�A�~�A�A�A�VA̾wAˡ�A��A��TAʰ!Aʏ\A�M�A�7LA�/A�/AɮA��A�I�A��HA�l�A��TA�x�A���A���A�bNAĉ7A�C�A�1'A��`A�-A���A���A��A�?}A�A��!A��PA�x�A�E�A���A���A�r�A�E�A� �A�r�A���A���A��A��A�`BA�(�A�A��PA���A�jA��A�E�A��PA�/A�G�A� �A��;A� �A��A�"�A�1A�O�A��;A�^5A��hA���A���A�
=A�x�A�A�A�ƨA�/A��jA��A�A�33A�  A"�A~��A~bNA|�Ay��AvbAr�/Aq�ApM�AhbNAct�Ab�\AbE�Aa��A`�A^��A]�#AZ��AX��AX�AXbNAX�AV�9AT9XAR��AM�mAI?}AGVACdZABJAA��AA�A@  A=�A;%A:1'A7�A5XA4�!A3�;A2�A1ƨA0^5A.�jA.�A,��A+�#A*�/A*�A)VA't�A%�FA"��A�hA�A��A�A��A�9AdZAS�A��A7LAdZA�`AbNA��A+A��AA�A|�A�!AZAƨA5?A
��A
�+A
  A��A-A1A�;A�
A�wAdZA�+A��A-AS�A�A�@���@��@�bN@�Z@�Q�@���@���@�n�@�X@�K�@��`@�Q�@ם�@��#@���@���@ӍP@�dZ@�C�@���@�$�@љ�@���@�K�@���@��;@ˮ@�t�@�K�@�;d@�@ʏ\@ȓu@�ff@��@�&�@�x�@�?}@��/@��@�ȴ@���@�`B@��`@���@���@�Z@�(�@�  @��m@��
@�|�@��R@�n�@�E�@�@���@�7L@��D@�r�@�I�@�(�@�o@�{@�J@��@�@���@��@�$�@�5?@��h@�/@�V@��-@��@��@��@���@�ff@�+@��@�I�@��u@��T@��#@�%@�"�@�1@���@�dZ@�C�@�;d@���@��m@�1'@�9X@� �@��@��9@��9@��9@���@�I�@���@�;d@�@��R@���@�v�@�=q@��@��#@���@���@��9@�bN@��@���@��P@�l�@�o@���@�J@��@�G�@�7L@��@���@��u@� �@�  @���@�"�@�v�@�M�@��@��@���@�p�@�G�@�j@���@��;@�  @�l�@���@�G�@��@�  @�  @��m@�ƨ@��P@�dZ@��@��w@��w@��F@�l�@��@��-@��@��u@���@�Q�@�j@� �@�n�@��`@�b@���@�z�@�Q�@�b@���@�1@�r�@�%@�`B@�x�@�/@��@�Ĝ@�Z@�b@��@��@�l�@�C�@�;d@�33@���@�r�@��w@�ƨ@�ȴ@�M�@�-@��-@��-@���@�9X@���@���@��T@��`@�Z@��@��@�S�@��@��+@�^5@�5?@�{@�J@�@�J@�J@��@��T@��#@��h@�7L@�V@�%@���@�_�@l�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
,B
,B
+B
+B
)�B
)�B
)�B
)�B
'�B
#�B
�B
VB
hB
hB
oB
�B
�B
�B
"�B
#�B
(�B
/B
2-B
5?B
?}B
E�B
K�B
W
B
[#B
]/B
bNB
cTB
aHB
dZB
iyB
|�B
�DB
��B
ƨB
�NBBDB�B1'B:^BVB[#BffBv�B�B|�B��B��B��B�'B�RB�-B�9B��BB\B{B�B �B"�B#�B$�B(�B/B0!B1'B1'B.B�BH�B\)B`BBaHB\)BT�BI�B.B�BuBDBJB��B�sB�HBǮB��Be`B33B �B�BB
�B
�B
��B
�XB
�B
��B
��B
��B
z�B
:^B
"�B
�B

=B	��B	�B	�B	�/B	ĜB	�B	��B	~�B	�1B	��B	\)B	6FB	.B	+B	$�B	�B	�B	oB	
=B	B	B	B	B��B��B�B�mB�)B�
B��BȴBƨBÖB��B��B��B��B��B��BǮB��B��B�BB�`B�sB�fB�mB�ZB�;B�ZB�ZB�NB�B��B�B�B�'B�!B�B��B��B��B�JB�VB�uB�{B��B��B��B��B��B��B��B��B��B��B�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B@��B�RB�XB�RB�^B��BB�}B�^B�FB�FB�LB�dB�jB�jB�jB�jB�jB�jB�jB�jB�^B�jB��BĜBǮB��B��B��B��B��B�B�B�B�B�#B�5B�HB�`B�ZB�`B�sB�B�B�B�B�B�B��B��B��B��B��B��B��B	  B	B	+B		7B	PB	\B	{B	�B	�B	�B	�B	�B	!�B	$�B	'�B	'�B	&�B	,B	49B	7LB	A�B	M�B	Q�B	M�B	VB	ffB	hsB	k�B	v�B	w�B	v�B	p�B	ffB	ffB	jB	n�B	o�B	t�B	y�B	� B	�B	�B	�1B	�\B	�bB	�hB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�'B	�-B	�9B	�FB	�FB	�LB	�LB	�XB	�XB	�XB	�^B	�^B	�XB	�XB	�^B	�dB	�jB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ÖB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	ȴB	ɺB	��B	��B	��B	��B	��B	��B	�B	�B	�)B	�/B	�5B	�BB	�HB	�HB	�NB	�NB	�NB	�NB	�HB	�HB	�;B	�/B	�)B	�/B	�/B	�)B	�)B	�#B	�5B	�HB	�ZB	�fB	�ZB	�ZB	�TB	�`B	�fB	�mB	�fB	�fB	�fB	�mB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	��B	��B
�B
�B
&f22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222224222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B
,B
,B
+B
+B
)�B
)�B
)�B
)�B
'�B
#�B
�B
VB
hB
hB
oB
�B
�B
�B
"�B
#�B
(�B
/B
2-B
5?B
?}B
E�B
K�B
W
B
[#B
]/B
bNB
cTB
aHB
dZB
iyB
|�B
�DB
��B
ƨB
�NBBDB�B1'B:^BVB[#BffBv�B�B|�B��B��B��B�'B�RB�-B�9B��BB\B{B�B �B"�B#�B$�B(�B/B0!B1'B1'B.B�BH�B\)B`BBaHB\)BT�BI�B.B�BuBDBJB��B�sB�HBǮB��Be`B33B �B�BB
�B
�B
��B
�XB
�B
��B
��B
��B
z�B
:^B
"�B
�B

=B	��B	�B	�B	�/B	ĜB	�B	��B	~�B	�1B	��B	\)B	6FB	.B	+B	$�B	�B	�B	oB	
=B	B	B	B	B��B��B�B�mB�)B�
B��BȴBƨBÖB��B��B��B��B��B��BǮB��B��B�BB�`B�sB�fB�mB�ZB�;B�ZB�ZB�NB�B��B�B�B�'B�!B�B��B��B��B�JB�VB�uB�{B��B��B��B��B��B��B��B��B��B��B�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B@��B�RB�XB�RB�^B��BB�}B�^B�FB�FB�LB�dB�jB�jB�jB�jB�jB�jB�jB�jB�^B�jB��BĜBǮB��B��B��B��B��B�B�B�B�B�#B�5B�HB�`B�ZB�`B�sB�B�B�B�B�B�B��B��B��B��B��B��B��B	  B	B	+B		7B	PB	\B	{B	�B	�B	�B	�B	�B	!�B	$�B	'�B	'�B	&�B	,B	49B	7LB	A�B	M�B	Q�B	M�B	VB	ffB	hsB	k�B	v�B	w�B	v�B	p�B	ffB	ffB	jB	n�B	o�B	t�B	y�B	� B	�B	�B	�1B	�\B	�bB	�hB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�'B	�-B	�9B	�FB	�FB	�LB	�LB	�XB	�XB	�XB	�^B	�^B	�XB	�XB	�^B	�dB	�jB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ÖB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	ȴB	ɺB	��B	��B	��B	��B	��B	��B	�B	�B	�)B	�/B	�5B	�BB	�HB	�HB	�NB	�NB	�NB	�NB	�HB	�HB	�;B	�/B	�)B	�/B	�/B	�)B	�)B	�#B	�5B	�HB	�ZB	�fB	�ZB	�ZB	�TB	�`B	�fB	�mB	�fB	�fB	�fB	�mB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	��B	��B
�B
�B
&f22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222224222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.13 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190510                              AO  ARCAADJP                                                                    20181005190510    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190510  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190510  QCF$                G�O�G�O�G�O�8000            