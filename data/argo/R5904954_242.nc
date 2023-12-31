CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:44Z creation      
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
resolution        =���   axis      Z        h  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  K�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  S`   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  e�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  mP   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  }�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �@   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �p   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �p   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �p   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �p   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005191744  20181005191744  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @������1   @���:�B@4�n��O��d�I�^5?1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�ff@���@���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B/��B7��B@  BH  BP  BXffB`  Bh  Bp  Bx  B�  B�  B�  B���B�  B�  B���B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B���B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C�fC�fC�fC�fC  C  C   C"�C$  C&  C(  C*  C,  C.  C/�fC2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CM�fCP  CR  CT  CV  CX  CZ�C\  C^  C`  Cb  Cd�Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C��3C��3C�  C�  C��C��C��C�  C��3C�  C��3C�  C��C�  C�  C�  C�  C��C��C��C�  C��3C�  C�  C�  C�  C�  C�  C��C��3C��3C��C��C�  C��C�  C��3C�  C�  C��C�  C�  C�  C��3C�  C��C��C��C��C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C��3C�  C�  C��C��C�  C��3C��3C�  C�  C�  C��C��C�  C��C�  C�  C��C��C��C�  C�  C�  C��3C��3C�  C��C��3C�  C��3C��C��3C��3C�  C�  C�  C�  C��3C�  C�  C�  D   D �fD  D� D  D� D  D� D  Dy�D  D�fD  D� DfD� D  D�fD	  D	� D
  D
� D  D�fD��D� D  D� DfD� D  D� D  Ds3DfD� D  Dy�D  D� D  D� D  D� D  D� D��D� DfD� D  D� D  D� D  D�fD  D� D  D� D��D� DfD� D   D �fD!  D!y�D!��D"� D#  D#y�D$  D$y�D%  D%� D&  D&y�D'  D'y�D(  D(� D)  D)� D*  D*� D+fD+� D,  D,� D-  D-� D.  D.� D/  D/� D0fD0� D1fD1� D2  D2� D3  D3� D4fD4� D4��D5� D6  D6� D7  D7� D8  D8� D9  D9� D:fD:� D;  D;� D<  D<� D=  D=� D>fD>y�D>��D?y�D@  D@� DA  DAy�DA��DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DGy�DH  DHs3DI  DI� DJ  DJ� DKfDK�fDL  DL� DM  DM� DM��DN� DO  DO� DO��DP�fDP��DQ� DR  DRy�DS  DSy�DT  DT� DU  DU� DV  DV� DW  DW� DXfDXy�DYfDY� DZ  DZ� DZ��D[� D\  D\� D\��D]� D^  D^� D^��D_�fD`fD`y�DafDa� Db  Db� Db��Dc�fDc��Dd� De  De� DffDf� Dg  Dg� Dh  Dh� Di  Di� DjfDj�fDk  Dk�fDk��Dl� DmfDm�fDn  Dny�DofDo� Dp  Dpy�Dp��Dqy�Dq��Dr� DsfDsy�Ds��Dt� DufDu�fDvfDv� Dv��Dwy�Dw� Dy�D�Q�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�\)@�A�HA$z�ADz�Adz�A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qB�B	�B�B�B!�B)�B0�RB8�RBA�BI�BQ�BY�Ba�Bi�Bq�By�B��\B��\B��\B�\)B��\B��\B�\)B��\B��\B��\B�\)B��\B��\B��\B��\B��\B��\Bď\Bȏ\B̏\BЏ\Bԏ\B�B�B��\B�\)B�\B�\B��\B�\B��\B��\C G�CG�CG�CG�CG�C
G�CG�CG�CG�CG�C.C.C.C.CG�CG�C G�C"aHC$G�C&G�C(G�C*G�C,G�C.G�C0.C2G�C4G�C6G�C8G�C:G�C<G�C>G�C@G�CBG�CDG�CFG�CHG�CJG�CLG�CN.CPG�CRG�CTG�CVG�CXG�CZaHC\G�C^G�C`G�CbG�CdaHCfG�ChG�CjG�ClG�CnG�CpG�CrG�CtG�CvG�CxG�CzG�C|G�C~G�C�#�C�#�C�#�C�
C�
C�#�C�#�C�0�C�0�C�0�C�#�C�
C�#�C�
C�#�C�0�C�#�C�#�C�#�C�#�C�0�C�0�C�0�C�#�C�
C�#�C�#�C�#�C�#�C�#�C�#�C�0�C�
C�
C�0�C�0�C�#�C�0�C�#�C�
C�#�C�#�C�0�C�#�C�#�C�#�C�
C�#�C�0�C�0�C�0�C�0�C�
C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�
C�
C�
C�
C�#�C�#�C�0�C�0�C�#�C�
C�
C�#�C�#�C�#�C�0�C�0�C�#�C�=qC�#�C�#�C�0�C�0�C�=qC�#�C�#�C�#�C�
C�
C�#�C�0�C�
C�#�C�
C�0�C�
C�
C�#�C�#�C�#�C�#�C�
C�#�C�#�C�#�D �D �RD�D��D�D��D�D��D�D��D�D�RD�D��DRD��D�D�RD	�D	��D
�D
��D�D�RD�D��D�D��DRD��D�D��D�D�DRD��D�D��D�D��D�D��D�D��D�D��D�D��DRD��D�D��D�D��D�D�RD�D��D�D��D�D��DRD��D �D �RD!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+RD+��D,�D,��D-�D-��D.�D.��D/�D/��D0RD0��D1RD1��D2�D2��D3�D3��D4RD4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:RD:��D;�D;��D<�D<��D=�D=��D>RD>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH�DI�DI��DJ�DJ��DKRDK�RDL�DL��DM�DM��DN�DN��DO�DO��DP�DP�RDQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DXRDX��DYRDY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_�RD`RD`��DaRDa��Db�Db��Dc�Dc�RDd�Dd��De�De��DfRDf��Dg�Dg��Dh�Dh��Di�Di��DjRDj�RDk�Dk�RDl�Dl��DmRDm�RDn�Dn��DoRDo��Dp�Dp��Dq�Dq��Dr�Dr��DsRDs��Dt�Dt��DuRDu�RDvRDv��Dw�Dw��Dw��Dy� D�Z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A�ȴA�ȴA�ƨAƝ�A�VA���AŴ9Aŧ�Aŉ7A��AľwAę�Ać+A�t�A�dZA�XA�C�A�$�A�1A���A��TA���A�ȴA���A���A���A���A���A���A���Aå�AÃA�S�A��A¸RA�AA�bNA�E�A��
A�A�p�A�%A��jA�-A��yA�{A�G�A�S�A�+A��A��#A�
=A�E�A��#A��HA�{A��uA��TA���A��A���A��TA���A�ffA��A��`A��A�/A�jA��A���A��;A�A�A�n�A���A�n�A�S�A�VA��!A�t�A�l�A�ffA�33A��-A�5?A�ZA��A�"�A��A��hA�z�A�  A��PA���A���A�bA��-A�oA�E�A��+A��A�$�A�ȴA���A�l�A~��Az�HAy�Atv�Arz�ApffAm�Af�DAd��Acx�Aa
=A_`BA_K�A]�A[�A[
=AZZAY33AX�9AXbNAW�AV��ATz�AR�AQ`BAO\)ALM�AIXAH(�AG�AG�AF��AF��AF1'ADȴACoAB��A?��A=33A<�A:$�A8�yA6��A5|�A3��A2Q�A1"�A0�!A/XA-VA+x�A*A)\)A'�#A&��A$Q�A#+A"�A!`BA v�A JA�A&�A�\AdZA��A��A�AVAr�A=qA�hA-AO�Av�A�A|�A�+A�AȴAM�A{A�A��A1A��A �AS�A
��A	O�A�uAƨA+A�uA�TA+AVA;dA�AA A�@���@��@�;d@�$�@�bN@��;@��T@��
@��m@ְ!@�9X@�dZ@���@���@���@�ƨ@ʰ!@�C�@��@��/@ÍP@�E�@���@���@�r�@��m@���@��P@�;d@�+@�o@��@��R@���@��!@�~�@�X@�I�@�-@�x�@��@�X@�X@���@�{@�-@�&�@�Q�@�=q@�%@���@���@�Z@��
@�\)@�;d@�"�@���@�ȴ@��+@��\@��!@��!@��!@�E�@��@�v�@�v�@�^5@�n�@�M�@�E�@�E�@�E�@�5?@�5?@�5?@�J@��@��@�@�{@�J@��T@���@�/@��9@�j@��
@�\)@���@�ff@��@���@��7@��h@��@�1@��@�
=@�$�@���@�&�@�r�@��
@�@���@�ff@��@���@��T@���@�?}@���@��u@�(�@� �@� �@�1'@�b@�ƨ@�+@�
=@��@��R@��+@�5?@��#@���@��7@�/@��@�\)@��!@��y@��R@��#@�@���@�G�@�&�@�7L@��@��@�Z@��m@��
@���@�|�@�C�@�@�ȴ@��R@��+@�M�@�$�@��#@�`B@��@��@�I�@�1@���@�l�@�\)@�@���@���@��@��#@���@�7L@�r�@��
@�@�n�@��+@�J@��h@�/@��9@��D@�bN@���@��@��P@�|�@�\)@��@���@���@�~�@�ff@�^5@�M�@�E�@���@��T@��@��@���@���@�x�@�p�@�X@�7L@�V@��j@�r�@�I�@�1@���@��P@�t�@�C�@�"�@�@��R@�5?@�@��@���@�x�@�G�@�%@���@��9@��D@�bN@�j@�A�@���@���@�|�@�o@��H@��R@���@�~�@�^5@�J@�@��@��T@��-@�hs@��@���@��u@�z�@�Z@��F@�+@�
=@��@��+@�n�@�E�@�5?@���@���@�5?@��-@���@�S�@s��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A�ȴA�ȴA�ƨAƝ�A�VA���AŴ9Aŧ�Aŉ7A��AľwAę�Ać+A�t�A�dZA�XA�C�A�$�A�1A���A��TA���A�ȴA���A���A���A���A���A���A���Aå�AÃA�S�A��A¸RA�AA�bNA�E�A��
A�A�p�A�%A��jA�-A��yA�{A�G�A�S�A�+A��A��#A�
=A�E�A��#A��HA�{A��uA��TA���A��A���A��TA���A�ffA��A��`A��A�/A�jA��A���A��;A�A�A�n�A���A�n�A�S�A�VA��!A�t�A�l�A�ffA�33A��-A�5?A�ZA��A�"�A��A��hA�z�A�  A��PA���A���A�bA��-A�oA�E�A��+A��A�$�A�ȴA���A�l�A~��Az�HAy�Atv�Arz�ApffAm�Af�DAd��Acx�Aa
=A_`BA_K�A]�A[�A[
=AZZAY33AX�9AXbNAW�AV��ATz�AR�AQ`BAO\)ALM�AIXAH(�AG�AG�AF��AF��AF1'ADȴACoAB��A?��A=33A<�A:$�A8�yA6��A5|�A3��A2Q�A1"�A0�!A/XA-VA+x�A*A)\)A'�#A&��A$Q�A#+A"�A!`BA v�A JA�A&�A�\AdZA��A��A�AVAr�A=qA�hA-AO�Av�A�A|�A�+A�AȴAM�A{A�A��A1A��A �AS�A
��A	O�A�uAƨA+A�uA�TA+AVA;dA�AA A�@���@��@�;d@�$�@�bN@��;@��T@��
@��m@ְ!@�9X@�dZ@���@���@���@�ƨ@ʰ!@�C�@��@��/@ÍP@�E�@���@���@�r�@��m@���@��P@�;d@�+@�o@��@��R@���@��!@�~�@�X@�I�@�-@�x�@��@�X@�X@���@�{@�-@�&�@�Q�@�=q@�%@���@���@�Z@��
@�\)@�;d@�"�@���@�ȴ@��+@��\@��!@��!@��!@�E�@��@�v�@�v�@�^5@�n�@�M�@�E�@�E�@�E�@�5?@�5?@�5?@�J@��@��@�@�{@�J@��T@���@�/@��9@�j@��
@�\)@���@�ff@��@���@��7@��h@��@�1@��@�
=@�$�@���@�&�@�r�@��
@�@���@�ff@��@���@��T@���@�?}@���@��u@�(�@� �@� �@�1'@�b@�ƨ@�+@�
=@��@��R@��+@�5?@��#@���@��7@�/@��@�\)@��!@��y@��R@��#@�@���@�G�@�&�@�7L@��@��@�Z@��m@��
@���@�|�@�C�@�@�ȴ@��R@��+@�M�@�$�@��#@�`B@��@��@�I�@�1@���@�l�@�\)@�@���@���@��@��#@���@�7L@�r�@��
@�@�n�@��+@�J@��h@�/@��9@��D@�bN@���@��@��P@�|�@�\)@��@���@���@�~�@�ff@�^5@�M�@�E�@���@��T@��@��@���@���@�x�@�p�@�X@�7L@�V@��j@�r�@�I�@�1@���@��P@�t�@�C�@�"�@�@��R@�5?@�@��@���@�x�@�G�@�%@���@��9@��D@�bN@�j@�A�@���@���@�|�@�o@��H@��R@���@�~�@�^5@�J@�@��@��T@��-@�hs@��@���@��u@�z�@�Z@��F@�+@�
=@��@��+@�n�@�E�@�5?@���@���@�5?@��-@���@�S�@s��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�'B�'B�'B�3B�dB�
B�B��B��B%B/BE�BK�BN�BQ�BT�BVBYB]/BaHBdZBe`BgmBgmBgmBgmBgmBgmBgmBgmBgmBjBn�Bt�B�B�DB�VB�bB�uB��B��B�B�B�!B�-B�3B�-B�B��B�B�FBB�dB�!B��B�B�3B�B�B��B��B��B�DB�Bx�Bo�BhsB`BBQ�BL�BB�B8RB+B�B{BB�B�sB�)B��BǮBĜBĜBÖB��B�^B��B��B�{B�=Bo�BdZB[#BB�B#�BVB
��B
��B
��B
�B
�5B
��B
�jB
�B
��B
l�B
T�B
C�B
%�B
�B	�B	�B	ŢB	�B	�B	z�B	v�B	s�B	u�B	~�B	�{B	�oB	�JB	�+B	�B	}�B	z�B	v�B	o�B	dZB	ZB	Q�B	F�B	7LB	(�B	"�B	 �B	�B	�B	�B	�B	bB	1B	%B��B�B�B�sB�TB�/B�B��B��B��BǮBB�qB�wB�jB�XB�?B�B��B��B��B��B��B��B��B��B��B��B��B�{B�bB�\B�VB�\B�\B�PB�DB�DB�=B�=B�1B�+B�%B�B�B�B�B�B� B� B~�B~�B� B� B�B�B�B�B~�B|�B~�B~�B~�B|�B|�B|�B{�By�BL�B�hB�{B��B��B��B��B��B��B��B��B��B�bB�PB�DB�PB�\B�bB�oB�uB��B��B��B��B��B��B��B��B��B��B��B��B�B�B��B�B�B�B�!B�FB�dB�wB��B�wB�XB�RB�RB�RB�^B�wBĜBŢBƨBȴB��B��B��B��B�
B�B�B�#B�HB�sB�B�B��B��B��B��B��B��B��B	  B	  B	B	+B		7B	
=B	
=B	\B	�B	�B	�B	�B	 �B	#�B	&�B	'�B	)�B	+B	,B	-B	5?B	9XB	;dB	@�B	B�B	F�B	J�B	L�B	S�B	VB	XB	ZB	[#B	[#B	]/B	`BB	aHB	dZB	ffB	ffB	ffB	hsB	iyB	l�B	q�B	r�B	r�B	u�B	v�B	x�B	z�B	{�B	|�B	~�B	�B	�B	�B	�%B	�+B	�+B	�7B	�DB	�PB	�\B	�bB	�bB	�bB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�'B	�-B	�9B	�?B	�9B	�FB	�FB	�FB	�?B	�9B	�3B	�3B	�FB	�LB	�LB	�RB	�XB	�^B	�^B	�^B	�dB	�dB	�jB	�qB	�qB	�}B	��B	��B	B	B	B	ÖB	ĜB	ƨB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�/B	�5B	�5B	�BB	�BB	�HB	�TB	�TB	�TB	�ZB	�`B	�fB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
  B	��B
�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B�'B�'B�'B�3B�dB�
B�B��B��B%B/BE�BK�BN�BQ�BT�BVBYB]/BaHBdZBe`BgmBgmBgmBgmBgmBgmBgmBgmBgmBjBn�Bt�B�B�DB�VB�bB�uB��B��B�B�B�!B�-B�3B�-B�B��B�B�FBB�dB�!B��B�B�3B�B�B��B��B��B�DB�Bx�Bo�BhsB`BBQ�BL�BB�B8RB+B�B{BB�B�sB�)B��BǮBĜBĜBÖB��B�^B��B��B�{B�=Bo�BdZB[#BB�B#�BVB
��B
��B
��B
�B
�5B
��B
�jB
�B
��B
l�B
T�B
C�B
%�B
�B	�B	�B	ŢB	�B	�B	z�B	v�B	s�B	u�B	~�B	�{B	�oB	�JB	�+B	�B	}�B	z�B	v�B	o�B	dZB	ZB	Q�B	F�B	7LB	(�B	"�B	 �B	�B	�B	�B	�B	bB	1B	%B��B�B�B�sB�TB�/B�B��B��B��BǮBB�qB�wB�jB�XB�?B�B��B��B��B��B��B��B��B��B��B��B��B�{B�bB�\B�VB�\B�\B�PB�DB�DB�=B�=B�1B�+B�%B�B�B�B�B�B� B� B~�B~�B� B� B�B�B�B�B~�B|�B~�B~�B~�B|�B|�B|�B{�By�BL�B�hB�{B��B��B��B��B��B��B��B��B��B�bB�PB�DB�PB�\B�bB�oB�uB��B��B��B��B��B��B��B��B��B��B��B��B�B�B��B�B�B�B�!B�FB�dB�wB��B�wB�XB�RB�RB�RB�^B�wBĜBŢBƨBȴB��B��B��B��B�
B�B�B�#B�HB�sB�B�B��B��B��B��B��B��B��B	  B	  B	B	+B		7B	
=B	
=B	\B	�B	�B	�B	�B	 �B	#�B	&�B	'�B	)�B	+B	,B	-B	5?B	9XB	;dB	@�B	B�B	F�B	J�B	L�B	S�B	VB	XB	ZB	[#B	[#B	]/B	`BB	aHB	dZB	ffB	ffB	ffB	hsB	iyB	l�B	q�B	r�B	r�B	u�B	v�B	x�B	z�B	{�B	|�B	~�B	�B	�B	�B	�%B	�+B	�+B	�7B	�DB	�PB	�\B	�bB	�bB	�bB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�'B	�-B	�9B	�?B	�9B	�FB	�FB	�FB	�?B	�9B	�3B	�3B	�FB	�LB	�LB	�RB	�XB	�^B	�^B	�^B	�dB	�dB	�jB	�qB	�qB	�}B	��B	��B	B	B	B	ÖB	ĜB	ƨB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�/B	�5B	�5B	�BB	�BB	�HB	�TB	�TB	�TB	�ZB	�`B	�fB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
  B	��B
�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.28 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191744                              AO  ARCAADJP                                                                    20181005191744    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191744  QCP$                G�O�G�O�G�O�F03E            AO  ARGQQCPL                                                                    20181005191744  QCF$                G�O�G�O�G�O�8000            