CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  ^   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:14Z creation      
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
resolution        =���   axis      Z        x  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 `  >�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     x  @H   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 `  E�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     x  G    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  L�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 `  R   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  Sp   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 `  X�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  ZH   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  _�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 `  e8   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  f�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 `  l   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  mp   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  r�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    s   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    v   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    y   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  |   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    |D   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    |H   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    |L   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    |P   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  |T   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    |�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    |�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    |�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         |�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         |�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        |�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    |�Argo profile    3.1 1.2 19500101000000  20181024140814  20181024140814  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               4A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @׼�H?�*1   @׼���� @3DZ�1�c��n��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      4A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A���A�  A�  A�  A�  B   B  BffBffB ffB(  B0  B8ffB@  BH  BP  BX  B_��Bh  Bp  Bx  B�  B�33B�  B�  B�  B�  B���B�  B�  B�  B�  B�33B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  C   C�fC  C  C  C
  C  C  C  C�C�C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C9�fC<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV�CX�CZ�C\�C^  C`  Cb  Cd  Cf  Cg�fCj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C��3C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fDfD� D  D� D   D � D!  D!y�D"  D"� D#  D#� D$fD$� D%  D%� D&  D&� D'  D'� D(  D(y�D)  D)� D*  D*� D*��D+� D,  D,y�D,��D-� D.  D.� Dy�qD�/\11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�\)@���Az�A$z�ADz�Adz�A�=qA�=qA�=qA�
>A�=qA�=qA�=qA�=qB�B	�B�B�B!�B)�B1�B9�BA�BI�BQ�BY�B`�RBi�Bq�By�B��\B�B��\B��\B��\B��\B�\)B��\B��\B��\B��\B�B��\B��\B�\)B�\)B��\Bď\Bȏ\B̏\BЏ\Bԏ\B؏\B܏\B��\B�\B�\B�\)B��\B�\B��\B��\C G�C.CG�CG�CG�C
G�CG�CG�CG�CaHCaHCG�CG�CG�CG�CG�C G�C"G�C$G�C&G�C(G�C*G�C,G�C.G�C0G�C2G�C4G�C6G�C8G�C:.C<G�C>G�C@G�CBG�CDG�CFG�CHG�CJG�CLG�CNG�CPG�CRG�CTG�CVaHCXaHCZaHC\aHC^G�C`G�CbG�CdG�CfG�Ch.CjG�ClG�CnG�CpG�CrG�CtG�CvG�CxG�CzG�C|G�C~G�C�#�C�#�C�#�C�#�C�#�C�
C�
C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�0�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�0�C�#�C�#�C�#�C�#�C�
C�
C�#�C�#�C�#�C�#�C�#�C�
C�
C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�0�C�#�C�#�C�#�C�#�C�#�C�#�C�0�C�0�C�#�C�#�C�#�C�
C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�
C�#�C�#�C�#�C�#�C�
C�
C�#�C�#�C�
C�#�C�#�C�#�C�0�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�0�C�#�C�
C�#�C�0�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D�RDRD��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$RD$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��Dy�]D�8R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�1A�A�A��/A�p�A��A���Aٰ!Aٙ�Aٕ�AًDAكAفAٍPA�x�A�hsA�ZA�O�A�A�A�1'A��A�JA�  A���A��mA���A�{Aִ9A���AӇ+A҉7A�+A��A�5?A��A̗�A� �A�G�A��yAˋDA�33A�"�A�A���A�7LAƁAě�A�bAhA�7LA��wA�C�A�  A�?}A�ĜA�|�A�G�A��A��A��\A�n�A�S�A��A���A�VA���A�^5A���A��yA�ffA��9A�E�A��!A��/A��^A�(�A�{A�A�A�7LA���A�%A��yA�+A��yA�M�A���A�5?A�A�dZA�^5A�|�A��+A�A��HA��jA��yA�XA�7LA���A�$�A���A��+A�~�A��A�A�ZA��A�\)A���A��uA�G�A�$�A�  A��RA�bNA�ƨA�O�A�$�A}�;A|��A{�hAyx�AxA�Av5?Ap�!Ao33Aml�AlbAkx�Ag�;Ad�yAc��AbbAa%A_�FA^�`A[XAX�AV5?AS7LAQ�AN��AN1'AM�#AL1'AJ��AIG�AE��A?t�A=A;��A6�uA3K�A2A1��A1A0�jA.ĜA-S�A+�A)�A&�A%�TA$�A!"�A%A�RA+AhsA�7A�A7LA9XAA��AS�A�mA&�A�A�`A^5A��Ap�A&�A  A��A�A$�A\)A��A5?A��Ax�A;dA"�A��AbA��A�A �AdZA"�A�jAv�A?}A	�A	S�A��A�;A��A��Ap�AG�A^5A�A�yA7LA~�A��A��A�A ~�A 1@��R@�X@��@�@���@�-@�&�@��y@�^5@�M�@�p�@�v�@�@��T@�5?@�@�P@��@�^@ݲ-@���@�Ĝ@�j@��m@�ff@�X@׾w@�V@�%@ԓu@���@�
=@���@ϥ�@�@��@�%@��;@ˍP@��H@�V@ɑh@���@Ǖ�@��@�{@��@�Ĝ@Ĵ9@���@�@+@�V@�J@�hs@�z�@��@��\@�~�@���@��@�"�@�ȴ@���@�$�@��@�@��@��`@���@��D@��@�z�@�bN@�9X@��@��P@�
=@���@��R@���@���@��+@�n�@�J@�O�@���@��9@�A�@��F@�;d@��R@�^5@���@���@��@�bN@�A�@�|�@�=q@��@�@��-@���@�hs@��@��F@�t�@�S�@�"�@���@�n�@�=q@�5?@�J@��^@���@���@���@��7@���@���@�Z@�bN@��;@��F@�a|@r�611111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�1A�A�A��/A�p�A��A���Aٰ!Aٙ�Aٕ�AًDAكAفAٍPA�x�A�hsA�ZA�O�A�A�A�1'A��A�JA�  A���A��mA���A�{Aִ9A���AӇ+A҉7A�+A��A�5?A��A̗�A� �A�G�A��yAˋDA�33A�"�A�A���A�7LAƁAě�A�bAhA�7LA��wA�C�A�  A�?}A�ĜA�|�A�G�A��A��A��\A�n�A�S�A��A���A�VA���A�^5A���A��yA�ffA��9A�E�A��!A��/A��^A�(�A�{A�A�A�7LA���A�%A��yA�+A��yA�M�A���A�5?A�A�dZA�^5A�|�A��+A�A��HA��jA��yA�XA�7LA���A�$�A���A��+A�~�A��A�A�ZA��A�\)A���A��uA�G�A�$�A�  A��RA�bNA�ƨA�O�A�$�A}�;A|��A{�hAyx�AxA�Av5?Ap�!Ao33Aml�AlbAkx�Ag�;Ad�yAc��AbbAa%A_�FA^�`A[XAX�AV5?AS7LAQ�AN��AN1'AM�#AL1'AJ��AIG�AE��A?t�A=A;��A6�uA3K�A2A1��A1A0�jA.ĜA-S�A+�A)�A&�A%�TA$�A!"�A%A�RA+AhsA�7A�A7LA9XAA��AS�A�mA&�A�A�`A^5A��Ap�A&�A  A��A�A$�A\)A��A5?A��Ax�A;dA"�A��AbA��A�A �AdZA"�A�jAv�A?}A	�A	S�A��A�;A��A��Ap�AG�A^5A�A�yA7LA~�A��A��A�A ~�A 1@��R@�X@��@�@���@�-@�&�@��y@�^5@�M�@�p�@�v�@�@��T@�5?@�@�P@��@�^@ݲ-@���@�Ĝ@�j@��m@�ff@�X@׾w@�V@�%@ԓu@���@�
=@���@ϥ�@�@��@�%@��;@ˍP@��H@�V@ɑh@���@Ǖ�@��@�{@��@�Ĝ@Ĵ9@���@�@+@�V@�J@�hs@�z�@��@��\@�~�@���@��@�"�@�ȴ@���@�$�@��@�@��@��`@���@��D@��@�z�@�bN@�9X@��@��P@�
=@���@��R@���@���@��+@�n�@�J@�O�@���@��9@�A�@��F@�;d@��R@�^5@���@���@��@�bN@�A�@�|�@�=q@��@�@��-@���@�hs@��@��F@�t�@�S�@�"�@���@�n�@�=q@�5?@�J@��^@���@���@���@��7@���@���@�Z@�bN@��;@��F@�a|@r�611111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��B
�B
�NB
�B
��B
ƨB
ÖB
B
B
B
ÖB
ŢB
��B
��B
��B
ȴB
��B
��B
ɺB
��B
��B
��B
��B
��B
��B
�ZB
��B)�B_;B]/BVB'�BW
B,B�B+Bn�B��B�B�LB�?B�9BǮB�HB��BVB%�B1'B>wBI�BQ�BVBdZBk�Bp�Bu�Bu�Bv�B|�B~�B�B�7B�JB�oB��B��B�uB��B��B��B��B��B�B�-B�LB��B�Bk�BbNBcTB>wB1'B.B�B�BVBJBhB�B&�B�BuB�B��B�yB�NBȴB�B��Bo�BP�BN�BA�B49B/B#�B �BuB
��B
�B
�B
�sB
�BB
ǮB
�VB
S�B
49B
#�B
�B
{B
+B	��B	�B	��B	��B	�FB	�B	��B	�uB	�B	x�B	o�B	hsB	_;B	XB	F�B	6FB	)�B	�B	hB		7B	B	B��B�B�B�)BǮB��B�RB�B��B�B�B��B��B��B��B��B�-B�3B�?B��B�bB�+B�DB�{B��B��B��B�B�B�B�B�B�!B�!B�3B�RB�qB�}BBƨB��BŢB�}B��B��B��B��B��B��B��BBB�}B�qB�dB�RB�qB�dB�RB�LB�XB�dB�LB�LB�jB�jB�dB�^B�XB�qB�jB�jB�jB�jB�jB�qB�wB�}B�}B��B��B�}B�wB��B��B��BĜBĜBÖB�}B�-B��B��B�B�!B�'B�!B�!B�LB�RB�RB�RB�RB�^B�dB�jB�wB��B��B��B��BÖBŢBƨBɺB��B��B��B�
B�B�)B�/B�BB�NB�fB�B�B�B��B��B��B��B��B��B	B	+B	
=B		7B	bB	hB	�B	�B	�B	�B	�B	�B	�B	#�B	%�B	%�B	&�B	&�B	&�B	'�B	(�B	)�B	-B	.B	.B	/B	.B	/B	/B	0!B	49B	6FB	7LB	8RB	;dB	=qB	?}B	?}B	@�B	C�B	C�B	C�B	C�B	H�B	P�B	S�B	XB	ZB	ZB	ZB	\)B	]/B	^5B	`BB	dZB	gmB	gmB	hsB	jB	k�B	p�B	}�B	�B	�B	�B	�1B	�1B	�+B	�+B	�%B	�1B
	B
'�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
��B
�B
�NB
�B
��B
ƨB
ÖB
B
B
B
ÖB
ŢB
��B
��B
��B
ȴB
��B
��B
ɺB
��B
��B
��B
��B
��B
��B
�ZB
��B)�B_;B]/BVB'�BW
B,B�B+Bn�B��B�B�LB�?B�9BǮB�HB��BVB%�B1'B>wBI�BQ�BVBdZBk�Bp�Bu�Bu�Bv�B|�B~�B�B�7B�JB�oB��B��B�uB��B��B��B��B��B�B�-B�LB��B�Bk�BbNBcTB>wB1'B.B�B�BVBJBhB�B&�B�BuB�B��B�yB�NBȴB�B��Bo�BP�BN�BA�B49B/B#�B �BuB
��B
�B
�B
�sB
�BB
ǮB
�VB
S�B
49B
#�B
�B
{B
+B	��B	�B	��B	��B	�FB	�B	��B	�uB	�B	x�B	o�B	hsB	_;B	XB	F�B	6FB	)�B	�B	hB		7B	B	B��B�B�B�)BǮB��B�RB�B��B�B�B��B��B��B��B��B�-B�3B�?B��B�bB�+B�DB�{B��B��B��B�B�B�B�B�B�!B�!B�3B�RB�qB�}BBƨB��BŢB�}B��B��B��B��B��B��B��BBB�}B�qB�dB�RB�qB�dB�RB�LB�XB�dB�LB�LB�jB�jB�dB�^B�XB�qB�jB�jB�jB�jB�jB�qB�wB�}B�}B��B��B�}B�wB��B��B��BĜBĜBÖB�}B�-B��B��B�B�!B�'B�!B�!B�LB�RB�RB�RB�RB�^B�dB�jB�wB��B��B��B��BÖBŢBƨBɺB��B��B��B�
B�B�)B�/B�BB�NB�fB�B�B�B��B��B��B��B��B��B	B	+B	
=B		7B	bB	hB	�B	�B	�B	�B	�B	�B	�B	#�B	%�B	%�B	&�B	&�B	&�B	'�B	(�B	)�B	-B	.B	.B	/B	.B	/B	/B	0!B	49B	6FB	7LB	8RB	;dB	=qB	?}B	?}B	@�B	C�B	C�B	C�B	C�B	H�B	P�B	S�B	XB	ZB	ZB	ZB	\)B	]/B	^5B	`BB	dZB	gmB	gmB	hsB	jB	k�B	p�B	}�B	�B	�B	�B	�1B	�1B	�+B	�+B	�%B	�1B
	B
'�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.28 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140814                              AO  ARCAADJP                                                                    20181024140814    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140814  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140814  QCF$                G�O�G�O�G�O�0               