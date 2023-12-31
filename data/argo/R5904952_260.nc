CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:06:04Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005190604  20181005190604  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL              A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��iwG z1   @��j��@1�ȴ9X�c�?|�h1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                     A   A   A   @@  @�  @�  @���A   A@  A`  A���A���A�  A�33A�  A�  A�33A�33B   BffBffBffB ffB(  B0  B8  B?��BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�33B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C�fC  C  C�fC  C  C  C  C   C"�C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:�C<  C>  C?�fCB  CD  CF  CH  CJ  CL  CN  CP  CR  CS�fCV  CX  CZ�C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp�Cr  Ct  Cv  Cx  Cz  C|  C~  C�fC��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C��C��C��C�  C�  C�  C�  D   D � D  D� D��D� D��D� D  D� D  Dy�D��D� D  D� D  D� D	  D	� D
  D
� D  D� D��Dy�D  D� D  D� D  D� D  D�fD  D� D  Dy�D��D� D  D� D  D� D  D�fD  D� DfD� D  D� D  Dy�D  D�fDfD� D��Dy�D��Dy�D��D� D fD �fD!fD!�fD"fD"�fD#fD#�fD$  D$� D%  D%� D%��D&� D'  D'� D(  D(� D)  D)�fD*fD*�fD+fD+�fD,  D,� D-  D-y�D-��D.y�D/  D/� D0fD0� D1  D1� D2  D2�fD3fD3� D3��D4� D5  D5� D6  D6� D7fD7� D8  D8� D9  D9� D:  D:� D:��D;� D<  D<�fD=  D=y�D>  D>� D?  D?� D@  D@�fDA  DA�fDBfDB� DC  DC�fDD  DD� DE  DE� DF  DFy�DG  DG�fDHfDH�fDIfDI�fDJfDJ�fDK  DKy�DL  DL�fDM  DM� DN  DN� DN��DO� DP  DP� DQ  DQ� DR  DRy�DS  DS�fDT  DT� DU  DU� DV  DV� DV��DWy�DW��DX� DYfDY� DZ  DZ� DZ��D[y�D\  D\�fD]  D]y�D]��D^y�D_  D_�fD`  D`� Da  Day�Db  Db�fDc  Dcy�Dc��Dd� DefDe� Df  Dfy�Df��Dg� Dh  Dh� Di  Di� Dj  Dj� DkfDk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do�fDp  Dp�fDq  Dq� Dr  Dr� Dr��Dsy�Dt  Dt� Du  Du� Dv  Dvy�Dw  Dw� Dy~D�I�D��
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @Q�@���@���A�HA$z�ADz�Adz�A�
>A�
>A�=qA�p�A�=qA�=qA�p�A�p�B�B	�B�B�B!�B)�B1�B9�B@�RBI�BQ�BY�Ba�Bi�Bq�By�B��\B��\B��\B��\B��\B��\B�B��\B��\B��\B��\B��\B�B��\B�\)B�\)B��\Bď\Bȏ\B̏\BЏ\Bԏ\B؏\B܏\B��\B�\B�\B�\B��\B�\B��\B��\C G�CG�CG�CG�CG�C
G�CG�CG�C.CG�CG�C.CG�CG�CG�CG�C G�C"aHC$G�C&G�C(G�C*G�C,G�C.G�C0G�C2G�C4G�C6G�C8G�C:aHC<G�C>G�C@.CBG�CDG�CFG�CHG�CJG�CLG�CNG�CPG�CRG�CT.CVG�CXG�CZaHC\G�C^G�C`G�CbG�CdG�CfG�ChG�CjG�ClG�CnG�CpaHCrG�CtG�CvG�CxG�CzG�C|G�C~G�C�
C�
C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�
C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�0�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�
C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�
C�#�C�#�C�#�C�#�C�
C�#�C�#�C�
C�#�C�#�C�0�C�#�C�#�C�#�C�#�C�#�C�
C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�
C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�0�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�
C�#�C�#�C�#�C�#�C�#�C�#�C�0�C�0�C�0�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�
C�#�C�#�C�#�C�#�C�0�C�0�C�0�C�0�C�#�C�#�C�#�C�#�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D�RD�D��D�D��D�D��D�D��D�D��D�D�RD�D��DRD��D�D��D�D��D�D�RDRD��D�D��D�D��D�D��D RD �RD!RD!�RD"RD"�RD#RD#�RD$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)�RD*RD*�RD+RD+�RD,�D,��D-�D-��D.�D.��D/�D/��D0RD0��D1�D1��D2�D2�RD3RD3��D4�D4��D5�D5��D6�D6��D7RD7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<�RD=�D=��D>�D>��D?�D?��D@�D@�RDA�DA�RDBRDB��DC�DC�RDD�DD��DE�DE��DF�DF��DG�DG�RDHRDH�RDIRDI�RDJRDJ�RDK�DK��DL�DL�RDM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS�RDT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DYRDY��DZ�DZ��D[�D[��D\�D\�RD]�D]��D^�D^��D_�D_�RD`�D`��Da�Da��Db�Db�RDc�Dc��Dd�Dd��DeRDe��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��DkRDk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do�RDp�Dp�RDq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dy� D�R�D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�A�A�?}A�C�A�K�A�K�A�M�A�M�A�M�A�K�A�K�A�M�A�M�A�O�A�O�A�Q�A�VA�XA�XA�VA�S�A�S�A�S�A�Q�A�Q�A�S�A�O�A�Q�A�M�A�C�A�ffA�^5A��;A�~�A�E�A�ffA�A�A��A�(�A�1'A�C�A�bNA�p�A�z�A��/A�&�A��A�v�A�~�A�"�A�S�A��-A���A��-A�p�A���A�;dA��PA��9A�+A�
=A�/A��A�bA�1'A��A��FA��7A���A�C�A�l�A���A��+A���A�Q�A��A�~�A�A�bNA��#A��A��A���A�ĜA�n�A��HA�1'A�&�A�O�A�oA���A��A���A���A��A�XA~-Az��AyAuhsArbAp�AnbAl-Ah��Ae+A`�jA^(�A\�9AY33AW�AUXATjAR�RAQK�AP=qAO�AN��AM�;AJ��AH~�AF$�AA�#A?�^A=x�A9��A8VA7�#A6bNA4�A4�A2�RA1x�A/��A.�!A-7LA,�DA,5?A+��A+7LA*v�A*�A)?}A%��A#�#A!��A A�A��AK�A�A�AVA|�A�RAA�A��A�-A�#A�AhsAȴA �A�A�^A�A^5A33AffA�RAbNAAC�A
=A��A�AdZA;dA7LA�RA��AJA+A	
=A�hA�A�A��A1'AM�AM�AVA�\AffA5?A=qAr�AA
=A�9A�A5?AM�A�A��AE�A\)A/A �A �+@��
@���@��9@�l�@�Q�@��@�1'@�@�@�9X@�|�@�\@�p�@�%@��@���@�+@�7L@�@���@蛦@�A�@�1@��/@���@���@�j@�hs@���@�@�K�@�@��@�^5@���@�Q�@�j@�t�@�-@�7L@�Ĝ@��
@�R@��#@�j@�b@�t�@�ȴ@��@ܬ@�1'@�S�@��@��H@��@ڰ!@�n�@�O�@�b@��;@ץ�@�@��H@�K�@�n�@��@թ�@�X@��@�b@�K�@��H@ҸR@Ұ!@ҏ\@�^5@�=q@�$�@�G�@�ƨ@�l�@�+@θR@·+@�ff@��@�x�@��@��@���@���@̼j@̴9@̬@�Z@�9X@��
@�t�@�+@���@��y@��@�~�@�-@��@ɲ-@��@�Ĝ@ț�@�A�@Ǯ@�;d@��@ư!@�ff@�J@ũ�@��@�j@��@Ý�@�o@���@�5?@��@��^@�O�@��@��P@��y@���@�^5@��^@�hs@�X@��@���@� �@�ƨ@��@�l�@�"�@�
=@�n�@���@�hs@�/@��9@��D@� �@���@�~�@��@���@���@�l�@�ƨ@���@��u@��9@��@��P@�^5@�5?@���@��-@�%@�r�@���@�I�@��m@��@���@��R@���@�n�@�5?@���@�7L@��j@�1'@�  @���@�@���@�$�@�x�@�Ĝ@���@�K�@�+@���@���@��\@�^5@���@���@���@��h@�Ĝ@��D@�bN@�9X@��@��@�33@���@���@�~�@�V@�J@��@�x�@��/@���@���@��D@�r�@�bN@�bN@�bN@�(�@�  @��@���@�|�@�dZ@�33@���@�V@���@��@��`@��9@�j@��@��w@���@��@�dZ@��@��@���@��^@���@��h@��@�x�@�hs@�G�@��@���@�z�@�A�@� �@�  @��m@�ƨ@�l�@�S�@�33@���@��!@�M�@�5?@��@��T@�7L@���@�r�@��@��;@��P@�"�@���@���@���@���@�^5@�J@�x�@���@��j@��@�(�@��w@��@���@s33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�A�A�?}A�C�A�K�A�K�A�M�A�M�A�M�A�K�A�K�A�M�A�M�A�O�A�O�A�Q�A�VA�XA�XA�VA�S�A�S�A�S�A�Q�A�Q�A�S�A�O�A�Q�A�M�A�C�A�ffA�^5A��;A�~�A�E�A�ffA�A�A��A�(�A�1'A�C�A�bNA�p�A�z�A��/A�&�A��A�v�A�~�A�"�A�S�A��-A���A��-A�p�A���A�;dA��PA��9A�+A�
=A�/A��A�bA�1'A��A��FA��7A���A�C�A�l�A���A��+A���A�Q�A��A�~�A�A�bNA��#A��A��A���A�ĜA�n�A��HA�1'A�&�A�O�A�oA���A��A���A���A��A�XA~-Az��AyAuhsArbAp�AnbAl-Ah��Ae+A`�jA^(�A\�9AY33AW�AUXATjAR�RAQK�AP=qAO�AN��AM�;AJ��AH~�AF$�AA�#A?�^A=x�A9��A8VA7�#A6bNA4�A4�A2�RA1x�A/��A.�!A-7LA,�DA,5?A+��A+7LA*v�A*�A)?}A%��A#�#A!��A A�A��AK�A�A�AVA|�A�RAA�A��A�-A�#A�AhsAȴA �A�A�^A�A^5A33AffA�RAbNAAC�A
=A��A�AdZA;dA7LA�RA��AJA+A	
=A�hA�A�A��A1'AM�AM�AVA�\AffA5?A=qAr�AA
=A�9A�A5?AM�A�A��AE�A\)A/A �A �+@��
@���@��9@�l�@�Q�@��@�1'@�@�@�9X@�|�@�\@�p�@�%@��@���@�+@�7L@�@���@蛦@�A�@�1@��/@���@���@�j@�hs@���@�@�K�@�@��@�^5@���@�Q�@�j@�t�@�-@�7L@�Ĝ@��
@�R@��#@�j@�b@�t�@�ȴ@��@ܬ@�1'@�S�@��@��H@��@ڰ!@�n�@�O�@�b@��;@ץ�@�@��H@�K�@�n�@��@թ�@�X@��@�b@�K�@��H@ҸR@Ұ!@ҏ\@�^5@�=q@�$�@�G�@�ƨ@�l�@�+@θR@·+@�ff@��@�x�@��@��@���@���@̼j@̴9@̬@�Z@�9X@��
@�t�@�+@���@��y@��@�~�@�-@��@ɲ-@��@�Ĝ@ț�@�A�@Ǯ@�;d@��@ư!@�ff@�J@ũ�@��@�j@��@Ý�@�o@���@�5?@��@��^@�O�@��@��P@��y@���@�^5@��^@�hs@�X@��@���@� �@�ƨ@��@�l�@�"�@�
=@�n�@���@�hs@�/@��9@��D@� �@���@�~�@��@���@���@�l�@�ƨ@���@��u@��9@��@��P@�^5@�5?@���@��-@�%@�r�@���@�I�@��m@��@���@��R@���@�n�@�5?@���@�7L@��j@�1'@�  @���@�@���@�$�@�x�@�Ĝ@���@�K�@�+@���@���@��\@�^5@���@���@���@��h@�Ĝ@��D@�bN@�9X@��@��@�33@���@���@�~�@�V@�J@��@�x�@��/@���@���@��D@�r�@�bN@�bN@�bN@�(�@�  @��@���@�|�@�dZ@�33@���@�V@���@��@��`@��9@�j@��@��w@���@��@�dZ@��@��@���@��^@���@��h@��@�x�@�hs@�G�@��@���@�z�@�A�@� �@�  @��m@�ƨ@�l�@�S�@�33@���@��!@�M�@�5?@��@��T@�7L@���@�r�@��@��;@��P@�"�@���@���@���@���@�^5@�J@�x�@���@��j@��@�(�@��w@��@���@s33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B+B,B,B,B+B,B+B+B,B+B,B+B+B,B+B+B+B+B+B,B,B,B,B,B,B,B,B,B+B1'B6FB6FBF�BVBn�B��B��B�B�B��B	DB	�B	 �B	+B	'�B	e`B	��B
(�B
�1B
�B#�BG�BffB]/BH�B/Bp�B�1B}�B^5BN�B?}B@�B�+B��B�B�'B�jB��B��BǮBƨB�^B�3B��Bu�Bl�B[#B1'B�BB
�fB
�!B
�PB
v�B
P�B
+B
B
B
	7B
uB
�B
�B
	7B	�B	�B	ĜB	�3B	��B	�oB	� B	u�B	ffB	XB	B�B	,B	�B	B��B�B�B�mB�fB�B�B��B��B�B	B	!�B	�B		7B�B�B�5BǮB�wB�XB�9B�3B�3B�3B�-B�3B�XB�}B�}B�}B�wB�jB�^B�LB�3B�!B�B�B�B�B�B�B�B�B�!B�9B�XB�}BÖB��B��B�fB�B��B�B��B	+B	,B	,B	"�B	hB��B��B��B��B��B��B	VB	oB	�B	�B	�B	oB		7B��B�B�B��B	B	1B	PB	PB	bB	�B	�B	 �B	'�B	,B	)�B	 �B	�B	�B		7B��B	JB	�B	�B	�B	"�B	#�B	#�B	"�B	�B	�B	�B	oB	JB	%B	B	  B��B��B	B	1B	+B	1B	DB	VB	bB	�B	!�B	%�B	+B	1'B	?}B	Q�B	hsB	z�B	�B	�PB	�DB	�VB	�oB	�uB	�hB	�\B	�oB	�{B	��B	��B	��B	��B	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�B	�?B	�qB	��B	B	ĜB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�
B	�
B	�
B	�B	�B	�B	�#B	�#B	�)B	�/B	�5B	�5B	�5B	�5B	�5B	�5B	�/B	�5B	�BB	�HB	�HB	�HB	�HB	�BB	�BB	�;B	�5B	�/B	�)B	�/B	�BB	�HB	�;B	�TB	�TB	�ZB	�fB	�fB	�fB	�fB	�`B	�`B	�`B	�fB	�fB	�fB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�sB	�ZB	�NB	�ZB	�mB	�TB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
1B
1B
1B
	7B
	7B
	7B
	7B
1B
1B
	7B

=B

=B

=B

=B
	7B
1B
1B
1B
+B
+B
%B
%B
B
%B
%B
B
B
B
B
B
B
%B
%B
%B
%B
%B
+B
1B
	7B

=B

=B

=B
JB
�B
B
*�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B+B,B,B,B+B,B+B+B,B+B,B+B+B,B+B+B+B+B+B,B,B,B,B,B,B,B,B,B+B1'B6FB6FBF�BVBn�B��B��B�B�B��B	DB	�B	 �B	+B	'�B	e`B	��B
(�B
�1B
�B#�BG�BffB]/BH�B/Bp�B�1B}�B^5BN�B?}B@�B�+B��B�B�'B�jB��B��BǮBƨB�^B�3B��Bu�Bl�B[#B1'B�BB
�fB
�!B
�PB
v�B
P�B
+B
B
B
	7B
uB
�B
�B
	7B	�B	�B	ĜB	�3B	��B	�oB	� B	u�B	ffB	XB	B�B	,B	�B	B��B�B�B�mB�fB�B�B��B��B�B	B	!�B	�B		7B�B�B�5BǮB�wB�XB�9B�3B�3B�3B�-B�3B�XB�}B�}B�}B�wB�jB�^B�LB�3B�!B�B�B�B�B�B�B�B�B�!B�9B�XB�}BÖB��B��B�fB�B��B�B��B	+B	,B	,B	"�B	hB��B��B��B��B��B��B	VB	oB	�B	�B	�B	oB		7B��B�B�B��B	B	1B	PB	PB	bB	�B	�B	 �B	'�B	,B	)�B	 �B	�B	�B		7B��B	JB	�B	�B	�B	"�B	#�B	#�B	"�B	�B	�B	�B	oB	JB	%B	B	  B��B��B	B	1B	+B	1B	DB	VB	bB	�B	!�B	%�B	+B	1'B	?}B	Q�B	hsB	z�B	�B	�PB	�DB	�VB	�oB	�uB	�hB	�\B	�oB	�{B	��B	��B	��B	��B	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�B	�?B	�qB	��B	B	ĜB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�
B	�
B	�
B	�B	�B	�B	�#B	�#B	�)B	�/B	�5B	�5B	�5B	�5B	�5B	�5B	�/B	�5B	�BB	�HB	�HB	�HB	�HB	�BB	�BB	�;B	�5B	�/B	�)B	�/B	�BB	�HB	�;B	�TB	�TB	�ZB	�fB	�fB	�fB	�fB	�`B	�`B	�`B	�fB	�fB	�fB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�sB	�ZB	�NB	�ZB	�mB	�TB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
1B
1B
1B
	7B
	7B
	7B
	7B
1B
1B
	7B

=B

=B

=B

=B
	7B
1B
1B
1B
+B
+B
%B
%B
B
%B
%B
B
B
B
B
B
B
%B
%B
%B
%B
%B
+B
1B
	7B

=B

=B

=B
JB
�B
B
*�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.28 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190604                              AO  ARCAADJP                                                                    20181005190604    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190604  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190604  QCF$                G�O�G�O�G�O�8000            