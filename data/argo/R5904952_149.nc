CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:38Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005190538  20181005190538  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @�ީ{r��1   @�ު���@0�9XbN�cۅ�Q�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�ff@�  A   AffA@  Aa��A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  BffB   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B��B���B�  B�  B�  B�  B�  B�  B�33B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C9�fC;�fC>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cu�fCx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C��C�  C��C��C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C��3C�  C�  C�  C��C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C��C��C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C��C��C�  C�  C��C�  C�  C�  C�  C�  C��C��C�  C��3C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  Dy�D  D�fD  D� D  D� D  D� D  D� D  D� DfD�fD	  D	� D
  D
� D  D� DfD�fD  D� D  Dy�D  D�fDfD�fDfD� D��D� DfD� D��D� DfD� D��Dy�D��D� DfD�fD  D� DfD� D  D� D  D� D  D� DfD�fD  Dy�D��D � D!  D!� D!��D"� D#  D#� D$fD$�fD%fD%� D%��D&� D&��D'y�D(  D(� D)  D)� D*  D*� D*��D+y�D,  D,� D-  D-� D.  D.� D/  D/� D0  D0y�D0��D1� D2  D2� D3  D3y�D4  D4� D4��D5y�D6  D6� D7  D7� D7��D8y�D9  D9� D:  D:� D;  D;� D<  D<� D=fD=� D>  D>� D>��D?� D@  D@y�DA  DA� DA��DB� DC  DC� DD  DD� DEfDE�fDFfDF�fDGfDG� DG��DHy�DH��DI� DJ  DJ�fDKfDK�fDL  DL� DMfDM�fDN  DNy�DOfDO�fDP  DP� DQ  DQ� DRfDR� DS  DS�fDT  DTy�DT��DU� DVfDV�fDW  DW�fDXfDX� DY  DY�fDZfDZ�fD[  D[� D\fD\�fD]fD]�fD^fD^�fD_  D_� D`  D`� D`��Da� Db  Db� Dc  Dc�fDd  Ddy�De  De�fDf  Df� DgfDg� Dg��Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dny�Do  Do�fDpfDp� Dq  Dqy�Dr  Dry�Ds  Ds�fDtfDt� Dt��Du� Dv  Dv� Dw  Dw� Dw� Dyq�D�J�D��H111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�\)@���Az�A"�GADz�Af{A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qB�B	�B�B�B!�B)�B1�B9�BA�BI�BQ�BY�Ba�Bi�Bq�By�B�\)B�\)B��\B��\B��\B��\B��\B��\B�B��\B�B��\B��\B��\B��\B��\B��\Bď\Bȏ\B̏\BЏ\B�B�B�B��\B�\B�\B�\B��\B�\B��\B��\C G�CG�CG�CG�CG�C
G�CG�CG�CG�CG�CG�CG�CG�CG�CG�CG�C G�C"G�C$G�C&G�C(G�C*G�C,G�C.G�C0G�C2G�C4G�C6G�C8G�C:.C<.C>G�C@G�CBG�CDG�CFG�CHG�CJG�CLG�CNG�CPG�CRG�CTG�CVG�CXG�CZG�C\G�C^G�C`G�CbG�CdG�CfG�ChG�CjG�ClG�CnG�CpG�CrG�CtG�Cv.CxG�CzG�C|G�C~G�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�0�C�#�C�
C�#�C�#�C�#�C�#�C�#�C�
C�
C�
C�#�C�#�C�#�C�#�C�0�C�#�C�0�C�0�C�#�C�#�C�#�C�0�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�
C�#�C�#�C�
C�
C�#�C�#�C�#�C�0�C�0�C�#�C�#�C�0�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�
C�#�C�0�C�0�C�0�C�#�C�
C�#�C�0�C�#�C�#�C�#�C�#�C�#�C�#�C�0�C�0�C�#�C�#�C�0�C�#�C�#�C�#�C�#�C�#�C�0�C�0�C�#�C�
C�
C�#�C�0�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�D �D ��D�D��D�D�RD�D��D�D��D�D��D�D��D�D��DRD�RD	�D	��D
�D
��D�D��DRD�RD�D��D�D��D�D�RDRD�RDRD��D�D��DRD��D�D��DRD��D�D��D�D��DRD�RD�D��DRD��D�D��D�D��D�D��DRD�RD�D��D �D ��D!�D!��D"�D"��D#�D#��D$RD$�RD%RD%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=RD=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DERDE�RDFRDF�RDGRDG��DH�DH��DI�DI��DJ�DJ�RDKRDK�RDL�DL��DMRDM�RDN�DN��DORDO�RDP�DP��DQ�DQ��DRRDR��DS�DS�RDT�DT��DU�DU��DVRDV�RDW�DW�RDXRDX��DY�DY�RDZRDZ�RD[�D[��D\RD\�RD]RD]�RD^RD^�RD_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc�RDd�Dd��De�De�RDf�Df��DgRDg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do�RDpRDp��Dq�Dq��Dr�Dr��Ds�Ds�RDtRDt��Du�Du��Dv�Dv��Dw�Dw��Dw��Dy��D�S�D��>111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�;dA�?}A�?}A�?}A�A�A�C�A�E�A�E�A�I�A�K�A�I�A�I�A�K�A�K�A�K�A�K�A�M�A�Q�A�VA�VA�VA�XA�VA�VA�VA�ZA�VA�XA�VA�ZA�ZA�ZA�ZA�ZA�ZA�\)A�bNA�ffA�dZA�dZA�hsA�hsA�l�A�l�A�l�A�jA�n�A�l�A�n�A�n�A�p�A�p�A�r�A�r�A�r�A�t�A�v�A�v�A�v�A�p�A�5?A�t�A̧�A��AǬA�^5A�+A��A��+A�S�A��A���A��
A���A�ĜA���A�%A�r�A��A�t�A���A�oA��
A�+A��A��A�ȴA�VA�A�A�A��`A��A�7LA��A�O�A��9A�ffA���A�  A���A���A�1A��mA���A��A�ƨA��^A��/A���A���A��+A� �A���A�Q�A}�Azv�Aw�#At��AqAmt�Ai�Af��Adz�Acl�Aa�A^��A\Q�AY�AW��AU\)AO��AN  AMhsAL�jAJȴAGAFJAA�A;C�A:�`A:�DA9x�A8^5A3�-A09XA.ffA,�/A,JA+��A*�jA(E�A%��A%O�A#?}A!"�A ZA^5AƨA�A^5At�AVA��A  A��A�7A�A��A1'A��A$�A�FAdZA�AĜAA�AĜA{A�;Ap�A
bNA	�wA��A��A�+A��A�+AM�AJA?}AȴAz�AI�A�FA33A v�A 9XA I�A -@�K�@���@�  @��w@�33@���@��@�&�@�9X@�33@�n�@�V@�j@��
@�K�@��;@�t�@�+@�p�@�%@�&�@��;@�{@��@�%@�Z@�P@旍@�@㕁@�ȴ@�\@�+@�V@���@�@���@�J@���@�{@ٙ�@�@�n�@�$�@�%@�dZ@�33@��@�ff@թ�@��/@���@��@���@��;@�ƨ@ӝ�@�l�@��@�^5@�1'@�I�@�C�@�$�@��@�=q@�V@͡�@�Ĝ@�b@�dZ@ʗ�@ʟ�@�v�@�{@���@��@�  @�C�@�o@Ƈ+@�v�@�v�@�@�Q�@Å@�dZ@��@�~�@�=q@���@�{@���@�o@�~�@�^5@�-@��@��@�Z@�1@��@���@��+@�@���@��^@���@�-@��-@���@��@�I�@�(�@���@�z�@�C�@��y@���@��H@���@��\@��@��T@��@��T@���@���@�X@�7L@��@��@���@�r�@�A�@��w@��P@�C�@��R@���@�p�@��/@��D@��@�1'@���@��@�+@��R@���@�?}@���@���@��9@�bN@��@��
@��@���@��P@�dZ@��H@���@���@�v�@�^5@�^5@�-@�x�@��`@�r�@�Q�@�  @���@�\)@�+@��y@��R@�=q@���@�X@�7L@�/@��@�%@���@� �@��m@���@�K�@���@���@�E�@��-@�/@���@�Ĝ@�I�@�A�@��m@���@��@�S�@�33@���@�n�@�M�@���@��7@�x�@�O�@���@�1@���@���@��P@�33@���@��@��@�G�@�/@�Ĝ@�r�@�bN@�  @���@�|�@�l�@���@�n�@�M�@�E�@�=q@�5?@��@���@�x�@�/@���@��j@� �@�S�@��H@�ȴ@��\@�V@�V@�@�O�@��@��9@�r�@�1'@���@���@��@�dZ@�K�@�C�@�33@�@�n�@���@��-@�X@�?}@�%@��@��j@��@�I�@�(�@���@���@��F@��@���@�t�@�\)@�"�@��H@���@�ff@�ff@�^5@�V@�V@�V@�V@�ff@�V@�E�@�J@���@�O�@��[@v�@d�)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�;dA�?}A�?}A�?}A�A�A�C�A�E�A�E�A�I�A�K�A�I�A�I�A�K�A�K�A�K�A�K�A�M�A�Q�A�VA�VA�VA�XA�VA�VA�VA�ZA�VA�XA�VA�ZA�ZA�ZA�ZA�ZA�ZA�\)A�bNA�ffA�dZA�dZA�hsA�hsA�l�A�l�A�l�A�jA�n�A�l�A�n�A�n�A�p�A�p�A�r�A�r�A�r�A�t�A�v�A�v�A�v�A�p�A�5?A�t�A̧�A��AǬA�^5A�+A��A��+A�S�A��A���A��
A���A�ĜA���A�%A�r�A��A�t�A���A�oA��
A�+A��A��A�ȴA�VA�A�A�A��`A��A�7LA��A�O�A��9A�ffA���A�  A���A���A�1A��mA���A��A�ƨA��^A��/A���A���A��+A� �A���A�Q�A}�Azv�Aw�#At��AqAmt�Ai�Af��Adz�Acl�Aa�A^��A\Q�AY�AW��AU\)AO��AN  AMhsAL�jAJȴAGAFJAA�A;C�A:�`A:�DA9x�A8^5A3�-A09XA.ffA,�/A,JA+��A*�jA(E�A%��A%O�A#?}A!"�A ZA^5AƨA�A^5At�AVA��A  A��A�7A�A��A1'A��A$�A�FAdZA�AĜAA�AĜA{A�;Ap�A
bNA	�wA��A��A�+A��A�+AM�AJA?}AȴAz�AI�A�FA33A v�A 9XA I�A -@�K�@���@�  @��w@�33@���@��@�&�@�9X@�33@�n�@�V@�j@��
@�K�@��;@�t�@�+@�p�@�%@�&�@��;@�{@��@�%@�Z@�P@旍@�@㕁@�ȴ@�\@�+@�V@���@�@���@�J@���@�{@ٙ�@�@�n�@�$�@�%@�dZ@�33@��@�ff@թ�@��/@���@��@���@��;@�ƨ@ӝ�@�l�@��@�^5@�1'@�I�@�C�@�$�@��@�=q@�V@͡�@�Ĝ@�b@�dZ@ʗ�@ʟ�@�v�@�{@���@��@�  @�C�@�o@Ƈ+@�v�@�v�@�@�Q�@Å@�dZ@��@�~�@�=q@���@�{@���@�o@�~�@�^5@�-@��@��@�Z@�1@��@���@��+@�@���@��^@���@�-@��-@���@��@�I�@�(�@���@�z�@�C�@��y@���@��H@���@��\@��@��T@��@��T@���@���@�X@�7L@��@��@���@�r�@�A�@��w@��P@�C�@��R@���@�p�@��/@��D@��@�1'@���@��@�+@��R@���@�?}@���@���@��9@�bN@��@��
@��@���@��P@�dZ@��H@���@���@�v�@�^5@�^5@�-@�x�@��`@�r�@�Q�@�  @���@�\)@�+@��y@��R@�=q@���@�X@�7L@�/@��@�%@���@� �@��m@���@�K�@���@���@�E�@��-@�/@���@�Ĝ@�I�@�A�@��m@���@��@�S�@�33@���@�n�@�M�@���@��7@�x�@�O�@���@�1@���@���@��P@�33@���@��@��@�G�@�/@�Ĝ@�r�@�bN@�  @���@�|�@�l�@���@�n�@�M�@�E�@�=q@�5?@��@���@�x�@�/@���@��j@� �@�S�@��H@�ȴ@��\@�V@�V@�@�O�@��@��9@�r�@�1'@���@���@��@�dZ@�K�@�C�@�33@�@�n�@���@��-@�X@�?}@�%@��@��j@��@�I�@�(�@���@���@��F@��@���@�t�@�\)@�"�@��H@���@�ff@�ff@�^5@�V@�V@�V@�V@�ff@�V@�E�@�J@���@�O�@��[@v�@d�)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B^5B]/B]/B]/B]/B]/B]/B]/B]/B^5B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B^5B^5B]/B^5B^5B^5B^5B^5B_;B_;B_;B^5B_;B^5B^5B^5B]/B\)BiyB}�B�VB�^BƨB��B�#B�/B�BB�TB�BB�sBBF�BR�BXBjBz�B}�B�B�Bu�Bl�B`BBVBM�B6FB'�B%�B!�B�B�B&�B�B%B�sB�LB�{B}�Bl�BL�B�B	7B
��B
��B
�B
��B
�JB
z�B
o�B
`BB
7LB
oB	�fB	B	��B	��B	�=B	u�B	_;B	M�B	E�B	;dB	6FB	/B	&�B	�B	�B	uB	B�B�B�B�B�mB�;B�BɺB��B�qB�jB�RB�'B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B��B�B�FB�RB�XB�^B�RB�?B�?B�FB�jBÖBȴB��B��B��B��B�B�
B�B�
B�
B�ZB�yB�B�B�sB�fB�TB�BB�5B�TB�`B�sB�B�sB�yB�sB�yB�B�B�B�B�B�B�B�fB�`B�yB�B�B�B�B�B�B��B��B��B��B��B��B��B��B�B��B��B	
=B	DB	
=B	
=B	
=B		7B	1B	
=B	VB	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	%�B	$�B	#�B	'�B	.B	33B	6FB	8RB	8RB	9XB	;dB	;dB	;dB	=qB	<jB	=qB	>wB	C�B	D�B	F�B	E�B	E�B	E�B	E�B	F�B	H�B	G�B	F�B	F�B	D�B	>wB	?}B	@�B	B�B	D�B	F�B	H�B	L�B	P�B	VB	[#B	^5B	^5B	gmB	m�B	s�B	u�B	x�B	x�B	x�B	y�B	y�B	{�B	�B	�+B	�B	�B	�%B	�7B	�JB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�-B	�3B	�3B	�3B	�3B	�3B	�?B	�FB	�FB	�FB	�FB	�XB	�^B	�dB	�qB	�wB	�wB	�wB	��B	��B	�}B	��B	ÖB	ĜB	ŢB	ƨB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�)B	�)B	�/B	�5B	�5B	�5B	�;B	�HB	�TB	�ZB	�ZB	�`B	�`B	�fB	�fB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
1B
1B
1B
	7B
	7B
	7B

=B

=B
DB
DB
JB
JB
JB
PB
VB
bB
hB
hB
oB
hB
oB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
$�B
)B
'�B
2G222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B^5B]/B]/B]/B]/B]/B]/B]/B]/B^5B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B^5B^5B]/B^5B^5B^5B^5B^5B_;B_;B_;B^5B_;B^5B^5B^5B]/B\)BiyB}�B�VB�^BƨB��B�#B�/B�BB�TB�BB�sBBF�BR�BXBjBz�B}�B�B�Bu�Bl�B`BBVBM�B6FB'�B%�B!�B�B�B&�B�B%B�sB�LB�{B}�Bl�BL�B�B	7B
��B
��B
�B
��B
�JB
z�B
o�B
`BB
7LB
oB	�fB	B	��B	��B	�=B	u�B	_;B	M�B	E�B	;dB	6FB	/B	&�B	�B	�B	uB	B�B�B�B�B�mB�;B�BɺB��B�qB�jB�RB�'B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B��B�B�FB�RB�XB�^B�RB�?B�?B�FB�jBÖBȴB��B��B��B��B�B�
B�B�
B�
B�ZB�yB�B�B�sB�fB�TB�BB�5B�TB�`B�sB�B�sB�yB�sB�yB�B�B�B�B�B�B�B�fB�`B�yB�B�B�B�B�B�B��B��B��B��B��B��B��B��B�B��B��B	
=B	DB	
=B	
=B	
=B		7B	1B	
=B	VB	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	%�B	$�B	#�B	'�B	.B	33B	6FB	8RB	8RB	9XB	;dB	;dB	;dB	=qB	<jB	=qB	>wB	C�B	D�B	F�B	E�B	E�B	E�B	E�B	F�B	H�B	G�B	F�B	F�B	D�B	>wB	?}B	@�B	B�B	D�B	F�B	H�B	L�B	P�B	VB	[#B	^5B	^5B	gmB	m�B	s�B	u�B	x�B	x�B	x�B	y�B	y�B	{�B	�B	�+B	�B	�B	�%B	�7B	�JB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�-B	�3B	�3B	�3B	�3B	�3B	�?B	�FB	�FB	�FB	�FB	�XB	�^B	�dB	�qB	�wB	�wB	�wB	��B	��B	�}B	��B	ÖB	ĜB	ŢB	ƨB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�)B	�)B	�/B	�5B	�5B	�5B	�;B	�HB	�TB	�ZB	�ZB	�`B	�`B	�fB	�fB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
1B
1B
1B
	7B
	7B
	7B

=B

=B
DB
DB
JB
JB
JB
PB
VB
bB
hB
hB
oB
hB
oB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
$�B
)B
'�B
2G222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.28 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190538                              AO  ARCAADJP                                                                    20181005190538    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190538  QCP$                G�O�G�O�G�O�F03E            AO  ARGQQCPL                                                                    20181005190538  QCF$                G�O�G�O�G�O�8000            