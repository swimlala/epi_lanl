CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T12:58:28Z creation      
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
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20181121125828  20190408133246  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5286                            2C  D   APEX                            6531                            072314                          846 @���8��(1   @����}D&@5\(��b陙���1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @���@�  A   AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B��B��B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B���B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct�Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt` Dy` D�	�D�VfD�p D��fD��D�VfD��3D���D�fD�L�D�� D���D�3D�S3DچfD���D�3D�6fD� D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�{@�z�A=qA ��AB=qAb=qA��A��A��A��A��A��A��A��B �\B�\B�\B\)B (�B(�\B0�\B8�\B@�\BH�\BP�\BX�\B`�\Bh�\Bp�\Bx�\B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�z�B�z�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�z�B�z�B�G�B�G�B�G�B�{B�G�B�G�B�G�B�G�C #�C#�C#�C#�C#�C
#�C#�C#�C#�C#�C#�C#�C#�C#�C#�C#�C #�C"#�C$#�C&#�C(#�C*#�C,#�C.#�C0#�C2#�C4#�C6#�C8#�C:#�C<#�C>#�C@#�CB#�CD#�CF#�CH#�CJ#�CL#�CN#�CP#�CR#�CT#�CV#�CX#�CZ#�C\#�C^#�C`#�Cb#�Cd#�Cf#�Ch#�Cj#�Cl#�Cn#�Cp#�Cr#�Ct=qCv#�Cx#�Cz#�C|#�C~#�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D\D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dth�Dyh�D�D�Z�D�t{D���D�!HD�Z�D���D��HD��D�QHD��{D��HD��D�W�Dڊ�D��HD��D�:�D�{D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AӑhAӓuAӏ\Aӗ�AӓuAӋDAӋDAӉ7AӍPA�z�A�v�A�v�A�l�A�hsA�ffA�hsA�ffA�+AҍPA�bNAЬAЋDA��`A��A��yA�1A̗�A�|�A��Aʕ�A�?}A�M�A��AɁA�ZA��A�XAƗ�A�S�A�AżjA�AÉ7A��A+A��;A�O�A�$�A�|�A��A�r�A�v�A��;A�A��A��DA�1A�^5A���A�M�A���A��mA���A�?}A��A�jA�K�A���A�O�A�-A��`A�t�A���A�t�A���A���A�A���A�z�A���A�l�A��A�1'A�Q�A�G�A�G�A��+A���A���A��TA�ĜA���A�JA��mA�VA�K�A��/A�=qA�?}A���A���A��yA�~�A�JA��uA�JA���A���A���A��A��A� �A���A���A�+A��HA��A{�AzbNAwoAu�At�uAq�Am�PAjQ�Ahz�AfffAe�AdAb �A[�;AV�yATffAQ�AO��AMdZAKC�AI+AG��AF��AD(�AA�;A@�jA?&�A>�uA=�^A<~�A;XA:�A9�#A8��A8bA6�A5��A4��A3C�A1�hA/��A-�
A-C�A,I�A*�yA)t�A(��A'\)A%�^A%VA$�/A$��A$^5A"ȴA!p�A �9A {A;dA��A  A�RA�PA&�AĜAr�A�TA��A�A&�A=qA`BAz�AVA-AO�AA��AG�A�yAffAl�A�jA$�A%A �AG�A	O�A�FAG�A�AO�A�DA�A��A�^A��A�wAdZAS�AK�A ��A r�@�;d@��T@���@�n�@�G�@�@�x�@���@�\)@�33@�@�-@�ff@�&�@�?}@�&�@�@�9@�33@��@��/@��@��@��T@�r�@�b@��m@�w@⟾@���@�x�@�&�@��`@� �@�
=@�=q@��@��;@ڏ\@�V@׶F@�"�@ՙ�@���@�ȴ@�=q@�@�G�@���@�J@���@��
@υ@�@�~�@��T@͡�@�X@��@���@� �@˶F@�C�@�-@�?}@�hs@��@���@�1@��
@�&�@���@��u@��@�o@�
=@�~�@�^5@��@���@��7@�I�@��u@���@���@��9@��9@���@�K�@�@��+@�5?@���@��#@��^@��h@��7@�p�@�7L@��@��@�(�@��m@��m@��@��@�S�@�;d@��@�ff@�{@��T@���@�X@��@��`@��D@�1'@���@��
@���@�;d@���@��\@��#@��@�7L@���@��@��/@���@�I�@�b@��;@�ƨ@��F@��@���@�dZ@�
=@��R@���@���@�n�@�E�@�-@���@�?}@��@�hs@���@��@�1@��w@���@��\@��!@��+@��@�X@�bN@��@���@���@�o@���@�ȴ@�C�@�o@�@�E�@��@�K�@��y@���@�ȴ@�ff@�V@��+@��@�dZ@�;d@��@�~�@�ff@�M�@���@�G�@���@��-@���@��j@�
=@��@�bN@�1@�|�@�@�l�@��P@���@���@���@���@���@���@��^@��7@�@�{@�5?@�$�@���@�G�@��@���@��/@�bN@��w@�dZ@�K�@�;d@�S�@�S�@�;d@��@��@���@�V@�@��#@��@�@��h@�7L@���@��@�I�@�A�@���@�ƨ@���@�|�@�;d@���@��R@���@��+@�n�@�^5@�$�@��#@��-@��h@�x�@�hs@�O�@�/@��@���@���@��u@�Q�@��@���@�\)@�;d@�o@���@���@��D@~�+@u`B@lZ@d�j@\j@U?}@M@E?}@=�h@7|�@2n�@+�m@%�@ ��@�/@bN@(�@E�@
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AӑhAӓuAӏ\Aӗ�AӓuAӋDAӋDAӉ7AӍPA�z�A�v�A�v�A�l�A�hsA�ffA�hsA�ffA�+AҍPA�bNAЬAЋDA��`A��A��yA�1A̗�A�|�A��Aʕ�A�?}A�M�A��AɁA�ZA��A�XAƗ�A�S�A�AżjA�AÉ7A��A+A��;A�O�A�$�A�|�A��A�r�A�v�A��;A�A��A��DA�1A�^5A���A�M�A���A��mA���A�?}A��A�jA�K�A���A�O�A�-A��`A�t�A���A�t�A���A���A�A���A�z�A���A�l�A��A�1'A�Q�A�G�A�G�A��+A���A���A��TA�ĜA���A�JA��mA�VA�K�A��/A�=qA�?}A���A���A��yA�~�A�JA��uA�JA���A���A���A��A��A� �A���A���A�+A��HA��A{�AzbNAwoAu�At�uAq�Am�PAjQ�Ahz�AfffAe�AdAb �A[�;AV�yATffAQ�AO��AMdZAKC�AI+AG��AF��AD(�AA�;A@�jA?&�A>�uA=�^A<~�A;XA:�A9�#A8��A8bA6�A5��A4��A3C�A1�hA/��A-�
A-C�A,I�A*�yA)t�A(��A'\)A%�^A%VA$�/A$��A$^5A"ȴA!p�A �9A {A;dA��A  A�RA�PA&�AĜAr�A�TA��A�A&�A=qA`BAz�AVA-AO�AA��AG�A�yAffAl�A�jA$�A%A �AG�A	O�A�FAG�A�AO�A�DA�A��A�^A��A�wAdZAS�AK�A ��A r�@�;d@��T@���@�n�@�G�@�@�x�@���@�\)@�33@�@�-@�ff@�&�@�?}@�&�@�@�9@�33@��@��/@��@��@��T@�r�@�b@��m@�w@⟾@���@�x�@�&�@��`@� �@�
=@�=q@��@��;@ڏ\@�V@׶F@�"�@ՙ�@���@�ȴ@�=q@�@�G�@���@�J@���@��
@υ@�@�~�@��T@͡�@�X@��@���@� �@˶F@�C�@�-@�?}@�hs@��@���@�1@��
@�&�@���@��u@��@�o@�
=@�~�@�^5@��@���@��7@�I�@��u@���@���@��9@��9@���@�K�@�@��+@�5?@���@��#@��^@��h@��7@�p�@�7L@��@��@�(�@��m@��m@��@��@�S�@�;d@��@�ff@�{@��T@���@�X@��@��`@��D@�1'@���@��
@���@�;d@���@��\@��#@��@�7L@���@��@��/@���@�I�@�b@��;@�ƨ@��F@��@���@�dZ@�
=@��R@���@���@�n�@�E�@�-@���@�?}@��@�hs@���@��@�1@��w@���@��\@��!@��+@��@�X@�bN@��@���@���@�o@���@�ȴ@�C�@�o@�@�E�@��@�K�@��y@���@�ȴ@�ff@�V@��+@��@�dZ@�;d@��@�~�@�ff@�M�@���@�G�@���@��-@���@��j@�
=@��@�bN@�1@�|�@�@�l�@��P@���@���@���@���@���@���@��^@��7@�@�{@�5?@�$�@���@�G�@��@���@��/@�bN@��w@�dZ@�K�@�;d@�S�@�S�@�;d@��@��@���@�V@�@��#@��@�@��h@�7L@���@��@�I�@�A�@���@�ƨ@���@�|�@�;d@���@��R@���@��+@�n�@�^5@�$�@��#@��-@��h@�x�@�hs@�O�@�/@��@���@���@��u@�Q�@��@���@�\)@�;d@�o@���@���@��D@~�+@u`B@lZ@d�j@\j@U?}@M@E?}@=�h@7|�@2n�@+�m@%�@ ��@�/@bN@(�@E�@
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
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
��B
��B
��B
��B
��B
�B
��BB#�BO�By�Bu�BS�BS�B`BB��B�B�yB�B+B�B%�BoB�BuB,BG�BC�B;dB)�BPBJB5?B<jBk�Bq�B�B�hB�Bs�Bl�BgmBaHBjBx�Bq�B�BÖB��B�BB�ZB�`B�mB�ZB�)B�B��B��B��B�B�)B�B�#B��BƨBĜBȴB�#B��B��B��B�B��B�Bv�Be`BT�B8RB$�B{B�BŢB�wB�!B�{B�JB�VBq�BC�B0!B&�B�B�B#�B,BE�B2-B�B	7B
�yB
�?B
�1B
jB
F�B
,B	�ZB	��B	��B	��B	��B	�DB	m�B	gmB	k�B	aHB	ZB	Q�B	E�B	"�B	+B��B�B�NB�B��BĜB�}B�XB�-B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�bB�\B�DB�DB�bB�{B�{B�uB�\B�=B�=B�1B�=B�=B�1B�%B�B� B{�By�Bu�Bt�Bs�Br�Bo�Bl�BhsBcTBbNB`BB^5B_;BaHBaHBbNBcTBgmBgmB`BBcTBffBdZBaHB_;B]/B\)BYBXBZB[#BhsBk�Bn�Bo�Bq�Bv�Bz�B}�Bz�Bz�Bz�Bz�By�B{�B�DB��B��B�uB�VB��B��B��B�B��B��B��B��B��B��B��B��B�B�'B�!B�9B�wB��BÖBǮB��B��B��B��B�
B�B�B�B�B�#B�B�B��B��B��B��B�
B�TB�B�B�B�B�B�B�B�B��B��B��B	  B	B	%B	%B	bB	{B	�B	�B	{B		7B��B�B�B�B�B�B�B�B�B��B��B	B	hB	�B	�B	�B	"�B	%�B	,B	/B	1'B	2-B	33B	33B	7LB	8RB	9XB	<jB	C�B	I�B	N�B	O�B	Q�B	Q�B	R�B	T�B	YB	[#B	^5B	`BB	bNB	dZB	e`B	gmB	iyB	k�B	k�B	n�B	p�B	r�B	v�B	w�B	y�B	z�B	}�B	�B	�B	�B	�%B	�7B	�JB	�JB	�VB	�VB	�\B	�bB	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�?B	�RB	�^B	�dB	�jB	�jB	�qB	��B	ÖB	ÖB	B	ÖB	ĜB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�)B	�)B	�)B	�5B	�BB	�HB	�HB	�NB	�NB	�NB	�TB	�TB	�ZB	�`B	�fB	�fB	�fB	�fB	�mB	�mB	�mB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B
B
VB
hB
�B
$�B
-B
33B
7LB
=qB
C�B
J�B
O�B
VB
\)B
`BB
cTB
hsB
l�B
r�B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
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
��B
��B
��B
��B
��B
��B
��BB#�BO�By�Bu�BS�BS�B`6B��B�B�mB�BB�B%�BaB}BgB+�BG�BC�B;ZB)�BCB=B56B<_BkzBq�B�B�\B�Bs�Bl|BgcBa;BjtBx�Bq�B�BÇB��B�5B�LB�UB�`B�NB�B��B��B��B��B�B�B�B�B��BƟBĔBȥB�B��B��B�|B��B�wB�Bv�BeTBT�B8GB$�BrB�uBŖB�fB�B�lB�?B�JBq�BC�B0B&�B~B�B#�B+�BE�B2B�B	(B
�lB
�/B
�$B
jpB
F�B
+�B	�LB	̿B	��B	��B	��B	�8B	m�B	g_B	kvB	a8B	ZB	Q�B	E�B	"�B	B��B�xB�>B� B̽BČB�mB�HB�B� B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�qB�pB�rB�^B�QB�MB�3B�3B�RB�mB�jB�gB�MB�,B�-B�B�,B�-B�!B�B� B�B{�By�Bu�Bt�Bs�Br�Bo�Bl}Bh`BcEBb=B`1B^#B_+Ba8Ba8Bb<BcDBg\Bg_B`1BcDBfVBdJBa8B_+B]B\BYBX BZB[BhaBkuBn�Bo�Bq�Bv�Bz�B}�Bz�Bz�Bz�Bz�By�B{�B�2B�sB��B�cB�DB�sB�}B��B��B��B��B��B��B��B��B��B��B�	B�B�B�'B�fB�wBÃBǞB̼B��B��B��B��B�B�B�B�B�B�B��B��B��B��B��B��B�EB�B�zB�B�B�B�B�B�B��B��B��B��B	�B	B	B	QB	lB	|B	uB	iB		&B��B�B�wB�yB�yB�mB�B�B�B��B��B	B	VB	~B	�B	�B	"�B	%�B	+�B	/B	1B	2B	3 B	3B	7;B	8AB	9FB	<WB	C�B	I�B	N�B	O�B	Q�B	Q�B	R�B	T�B	YB	[B	^ B	`/B	b<B	dIB	eOB	g[B	ifB	ktB	kqB	n�B	p�B	r�B	v�B	w�B	y�B	z�B	}�B	��B	�B	�B	�B	�%B	�:B	�8B	�DB	�EB	�HB	�OB	�RB	�VB	�cB	�pB	�vB	�uB	�vB	�{B	�uB	�mB	�hB	�zB	��B	��B	�|B	��B	��B	�uB	��B	��B	��B	��B	��B	��B	�xB	��B	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�{B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�B	��B	��B	�B	�.B	�@B	�NB	�QB	�XB	�WB	�aB	�pB	ÅB	ÃB	�|B	ÂB	ĉB	ƕB	ȢB	ʱB	ʮB	ʭB	̻B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�B	�B	�B	�"B	�/B	�5B	�6B	�;B	�;B	�;B	�BB	�CB	�HB	�NB	�TB	�VB	�RB	�UB	�\B	�ZB	�YB	�aB	�aB	�bB	�hB	�lB	�sB	�zB	�yB	�xB	�B	�B
B
EB
WB
�B
$�B
,�B
3 B
7;B
=aB
C�B
J�B
O�B
U�B
\B
`/B
c?B
h_B
lwB
r�B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.14 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904081332462019040813324620190408133246  AO  ARCAADJP                                                                    20181121125828    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121125828  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121125828  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190408133246  IP                  G�O�G�O�G�O�                