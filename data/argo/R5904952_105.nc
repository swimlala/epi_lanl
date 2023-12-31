CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:28Z creation      
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
resolution        =���   axis      Z        H  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ?�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     H  AL   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  G�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     H  I(   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  Op   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  U�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  WL   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  _(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  ep   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  k�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  mL   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  s�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  u(   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  {p   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    {�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ~�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �,   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �0   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �@   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �D   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �H   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �LArgo profile    3.1 1.2 19500101000000  20181005190528  20181005190528  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               iA   AO  6431                            2B  A   APEX                            7466                            062512                          846 @�ѧ��"1   @�ѧ�J6@1����F�c�V�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      iA   A   A   @@  @�  @�  A   A   A@  A`  A�  A�  A�33A�  A�  A�  A�  A���B   B  B��B��B��B(  B/��B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B���B�  B�  B�33B�  B���B���B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  C   C�C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C �C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C7�fC:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd�Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C��C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C��3C��3C��3C��3C�  C�  C��C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C��C�  C��3C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C��D   D y�D ��D� D  D� D  D� D  Dy�DfD� D  D� D  D� D  D�fD	  D	� D
fD
� DfD� D��Dy�D��Dy�D��Dy�D��D� DfD�fD  Dy�D  D� D  D�fDfD� D  D� D  Dy�D��Dy�D��Dy�D��Dy�D  D� D  D�fDfD� DfD� D  D� D  D� D   D y�D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(fD(� D)  D)� D)��D*� D+fD+� D,  D,� D-  D-� D.  D.y�D.��D/� D0fD0�fD1  D1y�D1��D2� D3fD3�fD4fD4� D5  D5� D5��D6� D7fD7�fD8fD8� D9  D9� D:  D:� D;  D;� D;��D<y�D<��D=y�D>  D>� D?  D?� D@fD@�fDAfDA� DB  DB� DC  DC� DD  DD�fDE  DE� DE��DF� DGfDG� DH  DH�fDH��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @N�R@�\)@�\)A�A#�AC�Ac�A��
A��
A�
=A��
A��
A��
A��
A��B �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B���B�u�B�u�B�u�B�B�B�u�B�u�B���B�u�B�B�B�B�B�u�B�u�B�u�B�u�B�u�B��B�u�B�u�B�u�B�u�B�u�B�u�B�u�C :�CT{C:�C:�C:�C
:�C:�C:�C:�C:�C:�C:�C:�C:�C:�C:�C T{C":�C$:�C&:�C(:�C*:�C,:�C.:�C0:�C2:�C4:�C6:�C8!GC::�C<:�C>:�C@:�CB:�CD:�CF:�CH:�CJ:�CL:�CN:�CP:�CR:�CT:�CV:�CX:�CZ:�C\:�C^:�C`:�Cb:�CdT{Cf:�Ch:�Cj:�Cl:�Cn:�Cp:�Cr:�Ct:�Cv:�Cx:�Cz:�C|:�C~:�C�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�*>C�*>C�qC�qC�qC�qC�qC�*>C�qC��C�qC�*>C�qC�qC�qC�qC�qC�qC�qC�qC�qC�*>C�*>C�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC��C�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�*>C�qC�qC�*>C�qC�qC�qC�qC��C��C��C��C�qC�qC�*>C�qC�qC�qC�*>C�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�*>C�*>C�*>C�qC�qC�qC�qC�qC�qC�qC�qC�*>C�qC�qC�qC�qC�*>C�qC�qC�*>C�qC��C�qC�qC�qC�*>C�*>C�qC�qC�qC�qC�qC�qC�qC�*>D �D �RDRD��D�D��D�D��D�D�RDD��D�D��D�D��D�D�D	�D	��D
D
��DD��DRD�RDRD�RDRD�RDRD��DD�D�D�RD�D��D�D�DD��D�D��D�D�RDRD�RDRD�RDRD�RD�D��D�D�DD��DD��D�D��D�D��D �D �RD!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(D(��D)�D)��D*RD*��D+D+��D,�D,��D-�D-��D.�D.�RD/RD/��D0D0�D1�D1�RD2RD2��D3D3�D4D4��D5�D5��D6RD6��D7D7�D8D8��D9�D9��D:�D:��D;�D;��D<RD<�RD=RD=�RD>�D>��D?�D?��D@D@�DADA��DB�DB��DC�DC��DD�DD�DE�DE��DFRDF��DGDG��DH�DH�DIR111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�dZA�bNA�jA�l�A�bNA�jA�n�A�n�A�jA�n�A�l�A�p�A�r�A�r�A�t�A�t�A�v�A�r�A�r�A�r�A�v�A�z�A�|�A�~�A�~�A�~�A�~�AځAځA�|�A�~�A�~�A�~�A�~�A�~�A�hsA��yAٶFAٝ�Aٕ�AٓuA�v�A��A�K�A�33AӲ-A�t�A�ȴA�x�Aȩ�A��A�G�A�x�A���A�I�A�|�A�1A��A�=qA�A��jA�XA���A�|�A�+A��\A���A�ZA��A��RA��\A���A��A��TA�-A�\)A���A�n�A��TA�$�A��;A� �A�
=A�`BA���A�I�A�~�A��A���A�r�A�M�A�ȴA�
=A��9A���A�E�A��\A���A��A��/A�^5A��A�9XA�Q�A��PA��#A��7A���A�"�A�oA�-A��
A��^A�ĜA���A���A��-A|I�Ay�-AvJAtbAs%Aq`BAo+Amp�Ak?}Ac��A^�A]�#AX��AS��AN�DAJ�jAG�TAE%AA�FA<bA9�A3��A1�wA0�HA/ƨA/G�A.I�A-O�A,��A,�\A,I�A+�^A+?}A);dA( �A'��A&�HA$�RA$$�A#��A#"�A"r�A �A ��A �Ax�A��A��AbA&�A�mAA�A  A��A
=A�A�!AO�A��A�A�;AA�A=qA�A33AȴAv�A�mAA$�AXA��A�A
=A
�jA
bA�+Ap�A~�A  AS�A7LA|�A��A%A��A�A��AbNA��A?}A �!A  �@�$�@��`@���@��y@�r�@��9@� �@�+@�hs@�l�@�!@�p�@�F@���@�D@�r�@�b@�S�@�
=@�-@�{@�V@���@���@ܼj@ڗ�@�^5@ڇ+@�-@�V@�+@�~�@Ձ@�%@�A�@ӝ�@�o@��@�O�@�j@�v�@���@�G�@���@ʧ�@��@ȓu@�bN@��@�l�@�l�@�dZ@�\)@��H@�E�@���@�9X@��@���@�v�@�=q@��@�x�@��/@�1'@��;@�C�@�
=@���@��@���@�?}@��j@��D@�1'@��
@�ƨ@��@���@���@�l�@�+@��y@��\@��T@��@�/@�%@�Ĝ@�j@�Q�@�1@���@�o@���@�n�@�@��^@��-@���@��7@�X@�&�@��@��@�  @��@�l�@��y@��@��h@�X@��@���@���@���@� �@��@�@���@�n�@���@��@�X@��@���@�bN@�b@��@�t�@�33@��H@��\@��#@���@�hs@�O�@�&�@�V@���@���@���@���@�t�@�"�@���@��!@�ff@�$�@��@�hs@�@��T@���@��`@��/@�r�@�1'@�A�@�V@��@���@��j@�j@���@��y@�5?@�x�@�p�@�`B@�X@�/@��@���@��@�I�@�ƨ@�|�@�t�@�dZ@�K�@�;d@�
=@��R@�n�@��-@�X@�/@��@�Z@�(�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�dZA�bNA�jA�l�A�bNA�jA�n�A�n�A�jA�n�A�l�A�p�A�r�A�r�A�t�A�t�A�v�A�r�A�r�A�r�A�v�A�z�A�|�A�~�A�~�A�~�A�~�AځAځA�|�A�~�A�~�A�~�A�~�A�~�A�hsA��yAٶFAٝ�Aٕ�AٓuA�v�A��A�K�A�33AӲ-A�t�A�ȴA�x�Aȩ�A��A�G�A�x�A���A�I�A�|�A�1A��A�=qA�A��jA�XA���A�|�A�+A��\A���A�ZA��A��RA��\A���A��A��TA�-A�\)A���A�n�A��TA�$�A��;A� �A�
=A�`BA���A�I�A�~�A��A���A�r�A�M�A�ȴA�
=A��9A���A�E�A��\A���A��A��/A�^5A��A�9XA�Q�A��PA��#A��7A���A�"�A�oA�-A��
A��^A�ĜA���A���A��-A|I�Ay�-AvJAtbAs%Aq`BAo+Amp�Ak?}Ac��A^�A]�#AX��AS��AN�DAJ�jAG�TAE%AA�FA<bA9�A3��A1�wA0�HA/ƨA/G�A.I�A-O�A,��A,�\A,I�A+�^A+?}A);dA( �A'��A&�HA$�RA$$�A#��A#"�A"r�A �A ��A �Ax�A��A��AbA&�A�mAA�A  A��A
=A�A�!AO�A��A�A�;AA�A=qA�A33AȴAv�A�mAA$�AXA��A�A
=A
�jA
bA�+Ap�A~�A  AS�A7LA|�A��A%A��A�A��AbNA��A?}A �!A  �@�$�@��`@���@��y@�r�@��9@� �@�+@�hs@�l�@�!@�p�@�F@���@�D@�r�@�b@�S�@�
=@�-@�{@�V@���@���@ܼj@ڗ�@�^5@ڇ+@�-@�V@�+@�~�@Ձ@�%@�A�@ӝ�@�o@��@�O�@�j@�v�@���@�G�@���@ʧ�@��@ȓu@�bN@��@�l�@�l�@�dZ@�\)@��H@�E�@���@�9X@��@���@�v�@�=q@��@�x�@��/@�1'@��;@�C�@�
=@���@��@���@�?}@��j@��D@�1'@��
@�ƨ@��@���@���@�l�@�+@��y@��\@��T@��@�/@�%@�Ĝ@�j@�Q�@�1@���@�o@���@�n�@�@��^@��-@���@��7@�X@�&�@��@��@�  @��@�l�@��y@��@��h@�X@��@���@���@���@� �@��@�@���@�n�@���@��@�X@��@���@�bN@�b@��@�t�@�33@��H@��\@��#@���@�hs@�O�@�&�@�V@���@���@���@���@�t�@�"�@���@��!@�ff@�$�@��@�hs@�@��T@���@��`@��/@�r�@�1'@�A�@�V@��@���@��j@�j@���@��y@�5?@�x�@�p�@�`B@�X@�/@��@���@��@�I�@�ƨ@�|�@�t�@�dZ@�K�@�;d@�
=@��R@�n�@��-@�X@�/@��@�Z@�(�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
ZB
[#B
[#B
[#B
[#B
[#B
ZB
ZB
[#B
ZB
ZB
[#B
[#B
[#B
\)B
\)B
[#B
`BB
}�B
�B
�B
�B
�B
�B
�1B
�JB
�uB
��B
�B
�3B
��BoB=qBN�Be`Bz�B�B�BǮB��B��B�
B�B�B�B��B��B��B1BVBuB{B�B�B'�B/B/B0!B'�B'�B�B�B�BVBB��B�sB��B�wB��B�%Br�Bk�Bl�BXBN�BC�B6FB,B!�B�BPB
��B
�NB
��B
�wB
��B
�'B
��B
�oB
{�B
jB
^5B
M�B
B�B
7LB
-B
%�B
�B	��B	�yB	�B	ȴB	��B	�3B	��B	��B	��B	dZB	9XB	)�B	VB��B�BB��B��BȴBŢB�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�LB�9B��B�%B{�Bw�Bx�Bs�Bs�Bs�Bz�B�B�9BB��B��B��B��BǮB�B�)B�#B�)B�B�)B�/B�B�B�
B��B��B��B��B��B��BÖB�}B��B��B�B�;B�;B�BB�ZB�sB�B�B�B�B�B�B�mB�5B�mB�sB��B��B��B��B��B��B��B��B��B��B��B	B	B	+B	+B	B��B�yB�B��B��B��B��B��B��B��BɺB��B��B��B�B�5B�HB�ZB�fB�sB�fB�fB�sB�B��B��B��B��B��B��B��B��B��B��B	  B	+B	JB	PB	VB	VB	\B	bB	oB	�B	�B	�B	�B	�B	!�B	"�B	$�B	'�B	(�B	+B	-B	.B	/B	0!B	0!B	1'B	33B	49B	6FB	9XB	=qB	A�B	B�B	E�B	I�B	K�B	M�B	O�B	Q�B	R�B	S�B	XB	\)B	^5B	_;B	`BB	bNB	gmB	hsB	k�B	o�B	p�B	q�B	t�B	z�B	{�B	|�B	}�B	~�B	~�B	~�B	� B	�B	�B	�%B	�7B	�DB	�DB	�PB	�\B	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�3B	�9B	�LB	�XB	�XB	�RB	�^B	�^B	�dB	�qB	ŢB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�
B	�B	�B	�B	�B	�#B	�/B	�/B	�5B	�5B	�5B	�5B	�HB	�N222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
ZB
[#B
[#B
[#B
[#B
[#B
ZB
ZB
[#B
ZB
ZB
[#B
[#B
[#B
\)B
\)B
[#B
`BB
}�B
�B
�B
�B
�B
�B
�1B
�JB
�uB
��B
�B
�3B
��BoB=qBN�Be`Bz�B�B�BǮB��B��B�
B�B�B�B��B��B��B1BVBuB{B�B�B'�B/B/B0!B'�B'�B�B�B�BVBB��B�sB��B�wB��B�%Br�Bk�Bl�BXBN�BC�B6FB,B!�B�BPB
��B
�NB
��B
�wB
��B
�'B
��B
�oB
{�B
jB
^5B
M�B
B�B
7LB
-B
%�B
�B	��B	�yB	�B	ȴB	��B	�3B	��B	��B	��B	dZB	9XB	)�B	VB��B�BB��B��BȴBŢB�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�LB�9B��B�%B{�Bw�Bx�Bs�Bs�Bs�Bz�B�B�9BB��B��B��B��BǮB�B�)B�#B�)B�B�)B�/B�B�B�
B��B��B��B��B��B��BÖB�}B��B��B�B�;B�;B�BB�ZB�sB�B�B�B�B�B�B�mB�5B�mB�sB��B��B��B��B��B��B��B��B��B��B��B	B	B	+B	+B	B��B�yB�B��B��B��B��B��B��B��BɺB��B��B��B�B�5B�HB�ZB�fB�sB�fB�fB�sB�B��B��B��B��B��B��B��B��B��B��B	  B	+B	JB	PB	VB	VB	\B	bB	oB	�B	�B	�B	�B	�B	!�B	"�B	$�B	'�B	(�B	+B	-B	.B	/B	0!B	0!B	1'B	33B	49B	6FB	9XB	=qB	A�B	B�B	E�B	I�B	K�B	M�B	O�B	Q�B	R�B	S�B	XB	\)B	^5B	_;B	`BB	bNB	gmB	hsB	k�B	o�B	p�B	q�B	t�B	z�B	{�B	|�B	}�B	~�B	~�B	~�B	� B	�B	�B	�%B	�7B	�DB	�DB	�PB	�\B	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�3B	�9B	�LB	�XB	�XB	�RB	�^B	�^B	�dB	�qB	ŢB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�
B	�B	�B	�B	�B	�#B	�/B	�/B	�5B	�5B	�5B	�5B	�HB	�N222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.23 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190528                              AO  ARCAADJP                                                                    20181005190528    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190528  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190528  QCF$                G�O�G�O�G�O�8000            