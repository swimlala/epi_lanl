CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:04Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005191704  20181005191704  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               EA   AO  6557                            2B  A   APEX                            7468                            062512                          846 @���r1   @�����1"@5P�`A�7�d?|�h1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      EA   A   A   @�  @���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B��B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B���B�  B�  B�  B�33B�33B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C�C�C �C"  C$  C&  C(  C*  C,  C.  C0  C2  C3�fC6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CM�fCP  CQ�fCS�fCV  CX  CZ  C\  C^  C`  Cb�Cd  Cf  Ch�Cj  Cl  Cn  Co�fCr  Ct�Cv�Cx  Cy�fC{�fC~�C��C��C��C��3C�  C�  C�  C��C��C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C�  C�  C�  C��C��C��C�  C��3C��3C�  C��C��3C��3C��3C�  C�  C�  C��C��C�  C��C�  C��3C�  C�  C�  C�  C�  C��3C�  C��3C��3C�  C��C�  C��C��3C�  C�  C��3C��C�  C�  C�  C��3C��3C�  C�  C��3C��C�  C��fC�  C�  C�  C��C�  C�  C��C�  C��3C��3C��C��C��C�  C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C��3C�  C��C��C��C�  C��C�  C��C�  C��3C�  C��3C��3C�  C�  C�  C�  C�  C��3C��C��3D fD � DfD� D  D� D  D�fD  D� D��Dy�DfD� D  D� DfD� D	  D	�fD
  D
y�D  D� D��Dy�D��Ds3D�3Ds3D��D� DfD� D  D�fD  D� D  D�fDfD� D��Dy�D��D� DfD�fD��Dy�D��D� D��Dy�D  Dy�D  D� D��D� DfD� D  Dy�D��D � D!fD!� D"  D"�fD"��D#y�D#��D$y�D$��D%y�D&  D&�fD'�D'� D(  D(�fD)  D)y�D*  D*� D+  D+�fD,  D,� D-  D-� D.fD.� D/fD/� D0  D0� D1fD1y�D1��D2� D3  D3y�D4  D4y�D4��D5� D6  D6� D7  D7� D8  D8� D8��D9y�D9��D:� D;  D;y�D<fD<�fD=fD=��D>�D>� D>��D?�fD@  D@� DA  DA� DBfDB� DB��DC� DDfDDy�DD�3DE� DF  DFy�DG  DG� DHfDH� DIfDI� DJ  DJ� DJ��DKy�DL  DL� DM  DM� DN  DN� DN��DO� DO��DP� DP��DQy�DQ��DRy�DR��DSy�DS��DTs3DT�3DUy�DV  DV�fDWfDWy�DX  DX�fDYfDYy�DZ  DZ�fDZ��D[�fD[��D\�fD]fD]� D]��D^�fD_fD_s3D_��D`y�D`��Da�fDbfDby�Dc  Dc�fDc�3Dd�fDe  De� Df  Df� DgfDg�fDg��Dh� Di�Di�fDjfDj�fDk�Dk� Dk��Dl�fDmfDm�fDnfDn�fDo  Doy�DpfDp� Dq  Dq�fDq��Dr� Ds  Dsy�DtfDty�Du  Du�fDu��Dv� Dw�Dw� Dw� Dys�D�=D��)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@�Az�A$z�ADz�Adz�A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qB�B	�B�B�RB!�B)�B1�B9�BA�BI�BQ�BY�Ba�Bi�Bq�By�B��\B�\)B��\B��\B��\B�B�B��\B��\B��\B�\)B��\B��\B��\B��\B��\B��\Bď\Bȏ\B̏\BЏ\Bԏ\B؏\B�\)B��\B�\B�\B�\B��\B�\B��\B��\C G�CG�CG�CG�CG�C
G�CG�CG�CG�CG�CG�CG�CG�CG�CaHCaHC aHC"G�C$G�C&G�C(G�C*G�C,G�C.G�C0G�C2G�C4.C6G�C8G�C:G�C<G�C>G�C@G�CBG�CDG�CFG�CHG�CJG�CLG�CN.CPG�CR.CT.CVG�CXG�CZG�C\G�C^G�C`G�CbaHCdG�CfG�ChaHCjG�ClG�CnG�Cp.CrG�CtaHCvaHCxG�Cz.C|.C~aHC�0�C�0�C�0�C�
C�#�C�#�C�#�C�0�C�0�C�#�C�0�C�#�C�#�C�#�C�#�C�0�C�#�C�#�C�#�C�#�C�#�C�0�C�#�C�#�C�#�C�#�C�#�C�0�C�#�C�#�C�#�C�0�C�0�C�0�C�#�C�
C�
C�#�C�0�C�
C�
C�
C�#�C�#�C�#�C�0�C�0�C�#�C�0�C�#�C�
C�#�C�#�C�#�C�#�C�#�C�
C�#�C�
C�
C�#�C�0�C�#�C�0�C�
C�#�C�#�C�
C�0�C�#�C�#�C�#�C�
C�
C�#�C�#�C�
C�0�C�#�C�
=C�#�C�#�C�#�C�0�C�#�C�#�C�0�C�#�C�
C�
C�0�C�0�C�0�C�#�C�
C�#�C�#�C�#�C�#�C�#�C�
C�#�C�#�C�#�C�0�C�#�C�
C�#�C�0�C�0�C�0�C�#�C�0�C�#�C�0�C�#�C�
C�#�C�
C�
C�#�C�#�C�#�C�#�C�#�C�
C�0�C�
D RD ��DRD��D�D��D�D�RD�D��D�D��DRD��D�D��DRD��D	�D	�RD
�D
��D�D��D�D��D�D�DD�D�D��DRD��D�D�RD�D��D�D�RDRD��D�D��D�D��DRD�RD�D��D�D��D�D��D�D��D�D��D�D��DRD��D�D��D �D ��D!RD!��D"�D"�RD#�D#��D$�D$��D%�D%��D&�D&�RD'�D'��D(�D(�RD)�D)��D*�D*��D+�D+�RD,�D,��D-�D-��D.RD.��D/RD/��D0�D0��D1RD1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<RD<�RD=RD=��D>�D>��D?�D?�RD@�D@��DA�DA��DBRDB��DC�DC��DDRDD��DEDE��DF�DF��DG�DG��DHRDH��DIRDI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT�DUDU��DV�DV�RDWRDW��DX�DX�RDYRDY��DZ�DZ�RD[�D[�RD\�D\�RD]RD]��D^�D^�RD_RD_�D`�D`��Da�Da�RDbRDb��Dc�Dc�RDdDd�RDe�De��Df�Df��DgRDg�RDh�Dh��Di�Di�RDjRDj�RDk�Dk��Dl�Dl�RDmRDm�RDnRDn�RDo�Do��DpRDp��Dq�Dq�RDr�Dr��Ds�Ds��DtRDt��Du�Du�RDv�Dv��Dw�Dw��Dw��Dy��D�FD��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aں^Aں^AھwAھwAڼjAھwAڼjA���A���A���A�ĜA�ƨA�ȴA�ƨA�ƨA���AڬA�l�A�G�A׾wA�ȴAӰ!A҅A�ZA�I�A�  A�;dA�5?AȸRAǾwA�bA�A��`A���AĴ9Aě�A�S�A�A�A�dZA�x�A��A� �A�n�A��A�p�A�\)A�
=A��A��A�ƨA���A�v�A�hsA�&�A���A��A�VA��A�K�A�1'A�p�A�/A��A�O�A���A��
A�7LA�ƨA�jA���A��7A�oA�"�A�ȴA���A�p�A�dZA��/A�Q�A�"�A��`A��A��/A���A��9A��A��#A�-A�n�A�x�A���A��A��-A��A���A�r�A��A���A�p�A�-A��
A��!A���A�5?A�A~1'A}|�Az��Av�!As
=Ap5?Am�Ah��Ah-Ah �Af��Ac��A_G�A[�-AYC�AV1'AT��AS��AQ�PAN�RAK�AH�AFĜAES�AD$�AC�AB^5A@��A?�A=VA;�-A:�`A:~�A:M�A8��A7
=A5��A4�A3��A3A2bA0ffA/\)A.bNA,I�A+�FA+S�A*��A(��A&�jA%hsA$ȴA$Q�A#�7A"bA�AĜAJAK�A��A$�A^5A��A��A�-AhsAVA�+A�\Av�AhsA\)AG�A�A1AO�A
n�A	`BA�A�TA��AXA��A �A�AZA�#A�AĜA��A�A7LA 1@�5?@��@�  @�o@��!@��@�Ĝ@�Q�@�K�@�+@��@�n�@�5?@�?}@웦@�^@�(�@�1@�dZ@��@��@�hs@�G�@� �@��@�
=@��y@�=q@��@�5?@�E�@�M�@�~�@ާ�@�ȴ@���@ݲ-@���@�"�@�G�@�bN@׶F@�33@ָR@�v�@�7L@�1'@Ӆ@��@�=q@���@� �@�dZ@���@�=q@�7L@˶F@�dZ@���@�-@ɡ�@�V@ȼj@� �@��@Ǿw@�C�@�
=@��@Ƈ+@�5?@�$�@ź^@�X@���@Ĵ9@ċD@�bN@�  @§�@��@�  @���@���@�"�@���@�7L@�&�@�V@��@��j@��D@�A�@��@���@�^5@��@���@�p�@�/@���@�r�@�b@��;@��;@���@���@�=q@��T@���@��^@���@�?}@���@���@�  @��@�o@���@�$�@��-@�/@�z�@��F@�S�@��y@�M�@���@��@��9@��u@�bN@�"�@��@��T@��#@��#@���@�@���@�&�@��@�bN@�Q�@�A�@���@��P@�b@�9X@�Q�@��@���@��@�9X@�|�@�E�@�$�@�5?@�V@�M�@�E�@�E�@��#@�V@��D@��D@��@�z�@� �@��
@�S�@��/@�  @�K�@�@�@��@���@���@�V@��^@�7L@��j@�j@�9X@�1@���@��@�l�@�;d@���@�ȴ@��\@�v�@�{@���@��#@��T@�@�7L@���@���@��j@��j@���@��u@�(�@��@�l�@�"�@��@�ff@��@�@���@�%@�Ĝ@�A�@���@�\)@�;d@��@�@���@��@��R@���@�v�@�n�@�M�@�5?@��@�@���@��@��#@�@�G�@���@���@��u@�1'@��
@�1@�ƨ@�t�@�t�@�l�@�dZ@�o@��H@��@��!@�M�@�$�@��@���@��-@��-@���@�X@��@���@��9@�z�@���@�"�@���@�v�@�n�@�J@��@��-@�x�@�hs@�X@��@���@�Q�@�b@��@��w@��@���@�S�@�K�@�33@�
=@��@���@��+@�V@�{@��@��#@���@�V@��@y�X@g111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aں^Aں^AھwAھwAڼjAھwAڼjA���A���A���A�ĜA�ƨA�ȴA�ƨA�ƨA���AڬA�l�A�G�A׾wA�ȴAӰ!A҅A�ZA�I�A�  A�;dA�5?AȸRAǾwA�bA�A��`A���AĴ9Aě�A�S�A�A�A�dZA�x�A��A� �A�n�A��A�p�A�\)A�
=A��A��A�ƨA���A�v�A�hsA�&�A���A��A�VA��A�K�A�1'A�p�A�/A��A�O�A���A��
A�7LA�ƨA�jA���A��7A�oA�"�A�ȴA���A�p�A�dZA��/A�Q�A�"�A��`A��A��/A���A��9A��A��#A�-A�n�A�x�A���A��A��-A��A���A�r�A��A���A�p�A�-A��
A��!A���A�5?A�A~1'A}|�Az��Av�!As
=Ap5?Am�Ah��Ah-Ah �Af��Ac��A_G�A[�-AYC�AV1'AT��AS��AQ�PAN�RAK�AH�AFĜAES�AD$�AC�AB^5A@��A?�A=VA;�-A:�`A:~�A:M�A8��A7
=A5��A4�A3��A3A2bA0ffA/\)A.bNA,I�A+�FA+S�A*��A(��A&�jA%hsA$ȴA$Q�A#�7A"bA�AĜAJAK�A��A$�A^5A��A��A�-AhsAVA�+A�\Av�AhsA\)AG�A�A1AO�A
n�A	`BA�A�TA��AXA��A �A�AZA�#A�AĜA��A�A7LA 1@�5?@��@�  @�o@��!@��@�Ĝ@�Q�@�K�@�+@��@�n�@�5?@�?}@웦@�^@�(�@�1@�dZ@��@��@�hs@�G�@� �@��@�
=@��y@�=q@��@�5?@�E�@�M�@�~�@ާ�@�ȴ@���@ݲ-@���@�"�@�G�@�bN@׶F@�33@ָR@�v�@�7L@�1'@Ӆ@��@�=q@���@� �@�dZ@���@�=q@�7L@˶F@�dZ@���@�-@ɡ�@�V@ȼj@� �@��@Ǿw@�C�@�
=@��@Ƈ+@�5?@�$�@ź^@�X@���@Ĵ9@ċD@�bN@�  @§�@��@�  @���@���@�"�@���@�7L@�&�@�V@��@��j@��D@�A�@��@���@�^5@��@���@�p�@�/@���@�r�@�b@��;@��;@���@���@�=q@��T@���@��^@���@�?}@���@���@�  @��@�o@���@�$�@��-@�/@�z�@��F@�S�@��y@�M�@���@��@��9@��u@�bN@�"�@��@��T@��#@��#@���@�@���@�&�@��@�bN@�Q�@�A�@���@��P@�b@�9X@�Q�@��@���@��@�9X@�|�@�E�@�$�@�5?@�V@�M�@�E�@�E�@��#@�V@��D@��D@��@�z�@� �@��
@�S�@��/@�  @�K�@�@�@��@���@���@�V@��^@�7L@��j@�j@�9X@�1@���@��@�l�@�;d@���@�ȴ@��\@�v�@�{@���@��#@��T@�@�7L@���@���@��j@��j@���@��u@�(�@��@�l�@�"�@��@�ff@��@�@���@�%@�Ĝ@�A�@���@�\)@�;d@��@�@���@��@��R@���@�v�@�n�@�M�@�5?@��@�@���@��@��#@�@�G�@���@���@��u@�1'@��
@�1@�ƨ@�t�@�t�@�l�@�dZ@�o@��H@��@��!@�M�@�$�@��@���@��-@��-@���@�X@��@���@��9@�z�@���@�"�@���@�v�@�n�@�J@��@��-@�x�@�hs@�X@��@���@�Q�@�b@��@��w@��@���@�S�@�K�@�33@�
=@��@���@��+@�V@�{@��@��#@���@�V@��@y�X@g111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BBB��B��BBBBBB��B��BBBBB��B��B�wB�jB�LB�LB�qB�}BB��B�#B�B�B  BBJB�B/B2-B33B5?B9XBK�BffBl�Bu�B�=B��B��B�FB��B��B��B�)B�`B�sB�sB�fB�HB��BȴBȴB��B�jB�HB�ZB�NB�;B�)B�B��B��B�}B�!B��B�VBs�B`BBZBT�BF�B#�B�B�B%�B �BDB�B�B��B~�BffB;dB:^B-B#�B$�B$�B �B�B�B\B
�B
��B
�jB
�9B
��B
��B
�VB
{�B
n�B
e`B
O�B
/B
hB	�B	��B	�-B	�!B	�dB	�B	��B	w�B	_;B	N�B	:^B	/B	(�B	!�B	{B	B��B��B�B�B�sB�HB�B��B��B��BǮBƨBĜB�}B�^B�FB�9B�'B�'B�3B�9B�?B�FB�LB�LB�RB�?B�-B�B��B��B��B��B��B��B��B�oB�JB�B� Bw�Bt�Br�Bq�Bp�Bm�BhsB[#BS�BO�BO�BN�BM�BK�BH�BF�BD�BB�BB�BA�B@�B?}B>wBB�BH�BN�BYBgmBl�Br�Bv�Bo�Br�Bs�Bm�Br�Bu�Bv�Bw�Bw�Bw�Bw�Bx�By�Bx�Bu�Bs�Br�Bu�Bt�Bu�Bu�Bt�Bs�By�B�7B�oB�uB�uB��B��B��B��B��B��B��B�B�B��B��B�B�9B�LB�^B�jB�qB�jB�}BBƨB��B��B��B��B��B��B��B��B�
B�B�/B�;B�BB�HB�NB�`B�`B�`B�fB�fB�mB�yB�yB�yB�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B	  B	  B	  B	  B	  B	B	B	+B	
=B	PB	\B	bB	hB	{B	{B	�B	�B	�B	�B	�B	�B	 �B	!�B	!�B	#�B	%�B	'�B	+B	33B	6FB	9XB	=qB	@�B	C�B	E�B	I�B	O�B	Q�B	S�B	ZB	^5B	`BB	aHB	aHB	aHB	dZB	m�B	m�B	m�B	n�B	n�B	o�B	p�B	s�B	w�B	y�B	z�B	z�B	~�B	�+B	�\B	�bB	�hB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�B	�!B	�!B	�!B	�'B	�-B	�-B	�9B	�?B	�FB	�?B	�?B	�FB	�FB	�FB	�FB	�LB	�RB	�RB	�XB	�^B	�qB	�wB	�wB	B	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�)B	�5B	�5B	�BB	�NB	�TB	�TB	�ZB	�`B	�fB	�fB	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
+B
+B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
�B
~B
B
+Q222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  BBB��B��BBBBBB��B��BBBBB��B��B�wB�jB�LB�LB�qB�}BB��B�#B�B�B  BBJB�B/B2-B33B5?B9XBK�BffBl�Bu�B�=B��B��B�FB��B��B��B�)B�`B�sB�sB�fB�HB��BȴBȴB��B�jB�HB�ZB�NB�;B�)B�B��B��B�}B�!B��B�VBs�B`BBZBT�BF�B#�B�B�B%�B �BDB�B�B��B~�BffB;dB:^B-B#�B$�B$�B �B�B�B\B
�B
��B
�jB
�9B
��B
��B
�VB
{�B
n�B
e`B
O�B
/B
hB	�B	��B	�-B	�!B	�dB	�B	��B	w�B	_;B	N�B	:^B	/B	(�B	!�B	{B	B��B��B�B�B�sB�HB�B��B��B��BǮBƨBĜB�}B�^B�FB�9B�'B�'B�3B�9B�?B�FB�LB�LB�RB�?B�-B�B��B��B��B��B��B��B��B�oB�JB�B� Bw�Bt�Br�Bq�Bp�Bm�BhsB[#BS�BO�BO�BN�BM�BK�BH�BF�BD�BB�BB�BA�B@�B?}B>wBB�BH�BN�BYBgmBl�Br�Bv�Bo�Br�Bs�Bm�Br�Bu�Bv�Bw�Bw�Bw�Bw�Bx�By�Bx�Bu�Bs�Br�Bu�Bt�Bu�Bu�Bt�Bs�By�B�7B�oB�uB�uB��B��B��B��B��B��B��B�B�B��B��B�B�9B�LB�^B�jB�qB�jB�}BBƨB��B��B��B��B��B��B��B��B�
B�B�/B�;B�BB�HB�NB�`B�`B�`B�fB�fB�mB�yB�yB�yB�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B	  B	  B	  B	  B	  B	B	B	+B	
=B	PB	\B	bB	hB	{B	{B	�B	�B	�B	�B	�B	�B	 �B	!�B	!�B	#�B	%�B	'�B	+B	33B	6FB	9XB	=qB	@�B	C�B	E�B	I�B	O�B	Q�B	S�B	ZB	^5B	`BB	aHB	aHB	aHB	dZB	m�B	m�B	m�B	n�B	n�B	o�B	p�B	s�B	w�B	y�B	z�B	z�B	~�B	�+B	�\B	�bB	�hB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�B	�!B	�!B	�!B	�'B	�-B	�-B	�9B	�?B	�FB	�?B	�?B	�FB	�FB	�FB	�FB	�LB	�RB	�RB	�XB	�^B	�qB	�wB	�wB	B	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�)B	�5B	�5B	�BB	�NB	�TB	�TB	�ZB	�`B	�fB	�fB	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
+B
+B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
�B
~B
B
+Q222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.28 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191704                              AO  ARCAADJP                                                                    20181005191704    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191704  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191704  QCF$                G�O�G�O�G�O�8000            