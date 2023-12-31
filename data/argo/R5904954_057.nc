CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:01Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005191701  20181005191701  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               9A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @׽��:1   @׽�}'�6@5Z�G�{�c��+J1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      9A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBx  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  C   C  C  C  C  C
  C  C  C  C�fC  C  C�C  C�fC�fC   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C5�fC7�fC:  C<  C>  C@  CB  CD  CF�CH�CJ  CK�fCM�fCP  CQ�fCS�fCV  CX�CZ  C\  C^�C`�Cb  Cc�fCe�fCh  Cj  Ck�fCn  Cp  Cr�Ct  Cv  Cx  Cy�fC{�fC~  C�fC��3C�  C�  C��3C��3C��3C�  C�  C�  C��C�  C��3C�  C��C��3C��C��3C�  C��C�  C�  C��C�  C�  C�  C�  C��C��3C��C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C�  C�  C�  C��fC��3C��3C��3C�  C�  C�  C��3C��3C�  C�  C��C�  C��3C�  C��3C��3C�  C�  C�  C�  C��3C��3C��3C�  C�  C��C��C��C�  C��C�  C��3C��3C��3C�  C��C��C�  C�  C�  C��3C�  C��C�  C��3C��3C��3C��3C�  C��C�  C��C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C�  C�  C��C��C��C��C�  C��C�  C�  C��C�  C�  D   D � D ��Dy�D��D� DfD� D��D�fDfD� D  D� D��Dy�D��Dy�D��D	� D
  D
� DfD� D��D� D��Dy�D  D� DfD� D  D� D  D�fD  Dy�D  D�fDfD� DfD�fD  D� D��D� D  D� D��D� D  D� DfD�fDfD� D  D� DfD� D  D� D   D � D ��D!� D"fD"� D"��D#� D$fD$� D%fD%�fD&  D&y�D&��D'y�D'��D(� D)  D)�fD*  D*� D+  D+� D,fD,�fD-  D-� D.  D.��D/fD/� D0  D0y�D1  D1�fD2fD2� D3  D3� D4fD4� D5  D5�fD6  D6� D7  D7� D8  D8� D8��D9� D:  D:� D;  D;y�D<  D<� D=  D=y�D=��D>� D?fD?� D?��D@y�DA  DA�fDB  DB� DC  DCy�DC��DDy�DEfDE�fDE��DF�fDGfDG� DHfDH� DH��DI� DJ  DJ�fDK  DK� DK��DLy�DL��DM� DN  DN� DO�DO��DPfDP� DQ  DQy�DR  DR� DSfDS� DT  DT� DU  DU� DV  DVy�DV��DW� DXfDX� DY  DYy�DZ  DZ� D[  D[� D\  D\y�D]  D]� D]��D^y�D^��D_� D`fD`�fDa  Da� Da��Dby�Dc  Dc� Dd  Dd� De  De�fDf  Dfy�Dg  Dg� Dh  Dh� Dh��Di� Dj  Djy�Dk  Dk�fDlfDl�fDl��Dm� DnfDn� Do  Do�fDp  Dp�fDp��Dq� DrfDr� Ds  Ds�fDtfDt�fDufDu�fDv  Dv� DwfDw� DwٚDy��D�ED���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@���Az�A$z�ADz�Adz�A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qB�B	�B�B�B!�B)�B1�B9�BA�BI�BQ�BY�Ba�Bi�Bq�By�B��\B��\B��\B��\B��\B��\B��\B��\B�\)B�\)B��\B��\B��\B��\B��\B��\B��\Bď\Bȏ\B̏\BЏ\Bԏ\B؏\B܏\B��\B�\B�\B�\B�\)B�\B��\B��\C G�CG�CG�CG�CG�C
G�CG�CG�CG�C.CG�CG�CaHCG�C.C.C G�C"G�C$G�C&G�C(G�C*G�C,G�C.G�C0G�C2G�C4G�C6.C8.C:G�C<G�C>G�C@G�CBG�CDG�CFaHCHaHCJG�CL.CN.CPG�CR.CT.CVG�CXaHCZG�C\G�C^aHC`aHCbG�Cd.Cf.ChG�CjG�Cl.CnG�CpG�CraHCtG�CvG�CxG�Cz.C|.C~G�C�
C�
C�#�C�#�C�
C�
C�
C�#�C�#�C�#�C�0�C�#�C�
C�#�C�0�C�
C�0�C�
C�#�C�0�C�#�C�#�C�0�C�#�C�#�C�#�C�#�C�0�C�
C�0�C�
C�#�C�#�C�#�C�#�C�#�C�#�C�0�C�#�C�#�C�
C�#�C�#�C�#�C�
=C�
C�
C�
C�#�C�#�C�#�C�
C�
C�#�C�#�C�0�C�#�C�
C�#�C�
C�
C�#�C�#�C�#�C�#�C�
C�
C�
C�#�C�#�C�0�C�0�C�0�C�#�C�0�C�#�C�
C�
C�
C�#�C�0�C�0�C�#�C�#�C�#�C�
C�#�C�0�C�#�C�
C�
C�
C�
C�#�C�0�C�#�C�0�C�#�C�
C�#�C�#�C�#�C�#�C�
C�#�C�#�C�#�C�0�C�#�C�#�C�#�C�#�C�#�C�#�C�0�C�#�C�#�C�0�C�0�C�0�C�0�C�#�C�0�C�#�C�#�C�0�C�#�C�#�D �D ��D�D��D�D��DRD��D�D�RDRD��D�D��D�D��D�D��D	�D	��D
�D
��DRD��D�D��D�D��D�D��DRD��D�D��D�D�RD�D��D�D�RDRD��DRD�RD�D��D�D��D�D��D�D��D�D��DRD�RDRD��D�D��DRD��D�D��D �D ��D!�D!��D"RD"��D#�D#��D$RD$��D%RD%�RD&�D&��D'�D'��D(�D(��D)�D)�RD*�D*��D+�D+��D,RD,�RD-�D-��D.�D.��D/RD/��D0�D0��D1�D1�RD2RD2��D3�D3��D4RD4��D5�D5�RD6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?RD?��D@�D@��DA�DA�RDB�DB��DC�DC��DD�DD��DERDE�RDF�DF�RDGRDG��DHRDH��DI�DI��DJ�DJ�RDK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DPRDP��DQ�DQ��DR�DR��DSRDS��DT�DT��DU�DU��DV�DV��DW�DW��DXRDX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`RD`�RDa�Da��Db�Db��Dc�Dc��Dd�Dd��De�De�RDf�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk�RDlRDl�RDm�Dm��DnRDn��Do�Do�RDp�Dp�RDq�Dq��DrRDr��Ds�Ds�RDtRDt�RDuRDu�RDv�Dv��DwRDw��Dw�Dy�qD�ND���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aز-Aا�Aة�Aا�Aء�Aؗ�A�|�A؏\A�5?A�;dAҬA�?}A��TAиRA�v�A�-A��A���A�r�A�&�A΁A�E�A�oA���A�hsA̓uA�hsA���A��A�jA�A�r�Aŏ\A�ĜA�1A´9A���A�;dA��A�&�A�(�A�JA�+A�S�A� �A�S�A�`BA���A�"�A�v�A�&�A�oA��A��A�I�A��-A��A���A��A��9A�bNA��A��wA�&�A���A��A�33A��^A�"�A�K�A��A���A�A�C�A��RA�
=A�5?A�VA���A�jA���A�5?A�33A�E�A�ƨA��!A�?}A�bA��yA��FA�jA��A���A�v�A���A�ZA�|�A�(�A��A�O�A���A���A}�#A|ĜAy�-AtbNAr(�Al�yAj�Ad�yA`z�A^��A]��A\z�AZ��AY��AX�RAWC�AR5?AOO�AN��AM�#AK��AI/AG�AG�AFJAD�/AD�uAC��AB�!ABn�AB1'AA�wAA�A@�uA@ �A?�mA?�-A?�PA>��A=|�A;��A:(�A8~�A7�A7�A733A6ĜA5S�A5A2�yA1
=A/�-A/\)A.ĜA.5?A-�A-dZA+��A*�A*��A*9XA)�TA(�yA'hsA#|�A �/A�+A��A��A��A33AĜA1Az�A��A��Av�AJA"�A��A��A�7Ar�AA�A�A�A�Ax�AoA  A�yA&�AĜA�!A�RA�jA�A�TAC�A ^5@���@���@�`B@�?}@�`B@�&�@�(�@�33@���@�
=@���@���@��9@��D@���@�v�@�O�@��9@�(�@�;d@���@�$�@��@��y@��y@��@�ȴ@�\@�(�@���@���@��;@�t�@���@���@��@ڧ�@ى7@���@�1@�|�@�K�@֧�@�X@�  @�"�@���@�M�@�p�@��@�1@�ƨ@�o@Ώ\@�x�@��;@ʸR@�@�p�@�G�@���@Ǖ�@Ƈ+@�5?@�$�@��#@ċD@���@��
@Ý�@��y@�=q@��h@�?}@��@��u@�9X@�ƨ@��F@���@���@�|�@���@���@��7@�O�@�b@��@��+@�J@��^@��@�/@���@�1@��;@�ƨ@�t�@��@���@�n�@�^5@�V@�M�@�@��@�Q�@�  @���@��R@�n�@�$�@��T@��^@���@���@��@�p�@�hs@�/@��F@�l�@�K�@�
=@�J@��^@��^@��7@�?}@���@��9@��;@��P@�|�@�l�@�C�@�+@�"�@�
=@�o@���@���@�@�O�@��@��j@��u@�j@�  @��@�+@�^5@��@��@��@���@���@�@�V@��h@�G�@���@��j@�r�@�9X@�(�@�b@��w@���@�\)@���@�J@���@�x�@�r�@��;@��w@���@��@�K�@��@���@���@�v�@�V@�E�@��#@��@�`B@�&�@���@��@��w@�\)@�C�@�33@�+@���@��!@�^5@�5?@�-@�J@��@���@���@���@��@�`B@�/@���@���@�Q�@� �@�1@�  @��@��;@���@���@���@�ƨ@��w@��@���@�\)@���@��H@��H@��@���@�v�@�{@�J@��@��-@�hs@��@���@��j@���@��D@� �@��P@�S�@�"�@�o@��@���@�@�
=@�@��@��!@��\@�v�@�M�@��@�@�O�@��`@��@��@�j@�Q�@�(�@�b@���@��
@��w@�|�@�\)@�+@��H@�~�@�{@��T@�x�@�`B@��@��@�j@�b@��
@��w@��@���@��P@��@�l�@�S�@�33@��@���@y�@j��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aز-Aا�Aة�Aا�Aء�Aؗ�A�|�A؏\A�5?A�;dAҬA�?}A��TAиRA�v�A�-A��A���A�r�A�&�A΁A�E�A�oA���A�hsA̓uA�hsA���A��A�jA�A�r�Aŏ\A�ĜA�1A´9A���A�;dA��A�&�A�(�A�JA�+A�S�A� �A�S�A�`BA���A�"�A�v�A�&�A�oA��A��A�I�A��-A��A���A��A��9A�bNA��A��wA�&�A���A��A�33A��^A�"�A�K�A��A���A�A�C�A��RA�
=A�5?A�VA���A�jA���A�5?A�33A�E�A�ƨA��!A�?}A�bA��yA��FA�jA��A���A�v�A���A�ZA�|�A�(�A��A�O�A���A���A}�#A|ĜAy�-AtbNAr(�Al�yAj�Ad�yA`z�A^��A]��A\z�AZ��AY��AX�RAWC�AR5?AOO�AN��AM�#AK��AI/AG�AG�AFJAD�/AD�uAC��AB�!ABn�AB1'AA�wAA�A@�uA@ �A?�mA?�-A?�PA>��A=|�A;��A:(�A8~�A7�A7�A733A6ĜA5S�A5A2�yA1
=A/�-A/\)A.ĜA.5?A-�A-dZA+��A*�A*��A*9XA)�TA(�yA'hsA#|�A �/A�+A��A��A��A33AĜA1Az�A��A��Av�AJA"�A��A��A�7Ar�AA�A�A�A�Ax�AoA  A�yA&�AĜA�!A�RA�jA�A�TAC�A ^5@���@���@�`B@�?}@�`B@�&�@�(�@�33@���@�
=@���@���@��9@��D@���@�v�@�O�@��9@�(�@�;d@���@�$�@��@��y@��y@��@�ȴ@�\@�(�@���@���@��;@�t�@���@���@��@ڧ�@ى7@���@�1@�|�@�K�@֧�@�X@�  @�"�@���@�M�@�p�@��@�1@�ƨ@�o@Ώ\@�x�@��;@ʸR@�@�p�@�G�@���@Ǖ�@Ƈ+@�5?@�$�@��#@ċD@���@��
@Ý�@��y@�=q@��h@�?}@��@��u@�9X@�ƨ@��F@���@���@�|�@���@���@��7@�O�@�b@��@��+@�J@��^@��@�/@���@�1@��;@�ƨ@�t�@��@���@�n�@�^5@�V@�M�@�@��@�Q�@�  @���@��R@�n�@�$�@��T@��^@���@���@��@�p�@�hs@�/@��F@�l�@�K�@�
=@�J@��^@��^@��7@�?}@���@��9@��;@��P@�|�@�l�@�C�@�+@�"�@�
=@�o@���@���@�@�O�@��@��j@��u@�j@�  @��@�+@�^5@��@��@��@���@���@�@�V@��h@�G�@���@��j@�r�@�9X@�(�@�b@��w@���@�\)@���@�J@���@�x�@�r�@��;@��w@���@��@�K�@��@���@���@�v�@�V@�E�@��#@��@�`B@�&�@���@��@��w@�\)@�C�@�33@�+@���@��!@�^5@�5?@�-@�J@��@���@���@���@��@�`B@�/@���@���@�Q�@� �@�1@�  @��@��;@���@���@���@�ƨ@��w@��@���@�\)@���@��H@��H@��@���@�v�@�{@�J@��@��-@�hs@��@���@��j@���@��D@� �@��P@�S�@�"�@�o@��@���@�@�
=@�@��@��!@��\@�v�@�M�@��@�@�O�@��`@��@��@�j@�Q�@�(�@�b@���@��
@��w@�|�@�\)@�+@��H@�~�@�{@��T@�x�@�`B@��@��@�j@�b@��
@��w@��@���@��P@��@�l�@�S�@�33@��@���@y�@j��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B|�B}�B}�B}�B~�B~�B� B}�B~�B�hB�'B�3B�B�B��B��B��B��B��B��B��B��B��B��B�9BɺB�BB�BDB:^Bp�B}�B�PB��B�uB��B��B�jBB��B��B�B�fB��B�B��BB  B��B��B�B�B�B�B�fB�HB�B�mB�HB�B�
B�
B��B��BǮB�XB��B��B�JB�%B�B~�Bx�Bo�BgmBW
BD�B#�B�BuBB��B�B�\Bw�BhsB`BB^5B[#BW
BQ�B5?B+B
�mB
�5B
��B
ĜB
�'B
�bB
y�B
p�B
jB
S�B
G�B
0!B
JB	��B	�B	��B	��B	�B	u�B	l�B	e`B	]/B	T�B	L�B	B�B	-B	�B	�B	oB	%B��B�B�B�B�`B�ZB�BB�/B�#B�B�B��B��B��B��B��B��B��B��BŢBƨBǮBŢBŢBŢB��B�B��B��BȴBŢBĜBÖBB��B�}B�^B�?B�9B�-B�B��B��B�bB� Bw�Bs�Br�Bo�Bk�BjBiyBgmBgmBgmBgmBgmBe`BdZBcTB`BB`BBn�B�7B��B��B��B��B��B�bB��B��B��B��B��B��B�%B�B}�Bw�Bs�Bw�B}�B�B�%B�=B�\B�hB��B��B�'B�3B�3B�9B�RB�dB�jB�jB�qB�dB�-B�B�9B�9B�9B�3B�3B�!B�B�B�!B�'B�3B�FB�^B�dB�RB�RB�XB�^B�qB�wB�}BBÖBÖBBĜBŢB��B��B�B�)B�HB�`B�sB�B�B�B�B�B�B�B�B�B��B��B��B��B��B	B	B	B	%B	1B		7B	
=B	DB	DB	DB	JB	\B	hB	uB	uB	�B	�B	 �B	 �B	 �B	 �B	!�B	'�B	.B	/B	0!B	2-B	49B	5?B	7LB	8RB	8RB	8RB	9XB	@�B	D�B	F�B	J�B	S�B	W
B	^5B	aHB	cTB	dZB	e`B	gmB	k�B	n�B	n�B	r�B	v�B	w�B	z�B	~�B	�B	�B	�B	�B	�%B	�=B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�!B	�?B	�dB	�wB	��B	��B	��B	��B	B	B	ÖB	ĜB	ŢB	ƨB	ƨB	ƨB	ƨB	��B	��B	��B	��B	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�#B	�/B	�/B	�/B	�/B	�5B	�5B	�5B	�;B	�;B	�;B	�BB	�HB	�NB	�TB	�ZB	�ZB	�ZB	�ZB	�`B	�`B	�`B	�`B	�`B	�`B	�`B	�`B	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
%B
+B
+B
	7B

�B
B
($222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B|�B}�B}�B}�B~�B~�B� B}�B~�B�hB�'B�3B�B�B��B��B��B��B��B��B��B��B��B��B�9BɺB�BB�BDB:^Bp�B}�B�PB��B�uB��B��B�jBB��B��B�B�fB��B�B��BB  B��B��B�B�B�B�B�fB�HB�B�mB�HB�B�
B�
B��B��BǮB�XB��B��B�JB�%B�B~�Bx�Bo�BgmBW
BD�B#�B�BuBB��B�B�\Bw�BhsB`BB^5B[#BW
BQ�B5?B+B
�mB
�5B
��B
ĜB
�'B
�bB
y�B
p�B
jB
S�B
G�B
0!B
JB	��B	�B	��B	��B	�B	u�B	l�B	e`B	]/B	T�B	L�B	B�B	-B	�B	�B	oB	%B��B�B�B�B�`B�ZB�BB�/B�#B�B�B��B��B��B��B��B��B��B��BŢBƨBǮBŢBŢBŢB��B�B��B��BȴBŢBĜBÖBB��B�}B�^B�?B�9B�-B�B��B��B�bB� Bw�Bs�Br�Bo�Bk�BjBiyBgmBgmBgmBgmBgmBe`BdZBcTB`BB`BBn�B�7B��B��B��B��B��B�bB��B��B��B��B��B��B�%B�B}�Bw�Bs�Bw�B}�B�B�%B�=B�\B�hB��B��B�'B�3B�3B�9B�RB�dB�jB�jB�qB�dB�-B�B�9B�9B�9B�3B�3B�!B�B�B�!B�'B�3B�FB�^B�dB�RB�RB�XB�^B�qB�wB�}BBÖBÖBBĜBŢB��B��B�B�)B�HB�`B�sB�B�B�B�B�B�B�B�B�B��B��B��B��B��B	B	B	B	%B	1B		7B	
=B	DB	DB	DB	JB	\B	hB	uB	uB	�B	�B	 �B	 �B	 �B	 �B	!�B	'�B	.B	/B	0!B	2-B	49B	5?B	7LB	8RB	8RB	8RB	9XB	@�B	D�B	F�B	J�B	S�B	W
B	^5B	aHB	cTB	dZB	e`B	gmB	k�B	n�B	n�B	r�B	v�B	w�B	z�B	~�B	�B	�B	�B	�B	�%B	�=B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�!B	�?B	�dB	�wB	��B	��B	��B	��B	B	B	ÖB	ĜB	ŢB	ƨB	ƨB	ƨB	ƨB	��B	��B	��B	��B	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�#B	�/B	�/B	�/B	�/B	�5B	�5B	�5B	�;B	�;B	�;B	�BB	�HB	�NB	�TB	�ZB	�ZB	�ZB	�ZB	�`B	�`B	�`B	�`B	�`B	�`B	�`B	�`B	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
%B
+B
+B
	7B

�B
B
($222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.28 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191701                              AO  ARCAADJP                                                                    20181005191701    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191701  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191701  QCF$                G�O�G�O�G�O�8000            