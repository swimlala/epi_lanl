CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
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
resolution        =���   axis      Z        h  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   =�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  >�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   C\   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  Dx   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  H�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   MH   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  Nd   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   R�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  S�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  XP   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   \�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  ]�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   b<   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  cX   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  g�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    g�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    j�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    m�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  p�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    q   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    q    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    q$   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    q(   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  q,   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ql   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    q|   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    q�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         q�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         q�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        q�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    q�Argo profile    3.1 1.2 19500101000000  20181005191704  20181005191704  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               DA   AO  6557                            2B  A   APEX                            7468                            062512                          846 @��d�4h�1   @��eq�p@5T��E��dV�u1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      DA   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���B   B  B  B  B   B(  B0  B8  B@ffBHffBP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C�C
  C  C  C  C�fC  C�C�C  C  C  C   C"  C$  C&  C(  C)�fC+�fC.  C0�C2  C3�fC5�fC7�fC9�fC;�fC>  C@�CB  CC�fCF  CH�CI�fCK�fCN  CP  CQ�fCT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf�Ch  Cj  Cl  Cn�Cp�Cr�Ct  Cv�Cx  Cy�fC{�fC}�fC�  C��3C�  C�  C�  C��C�  C�  C��3C��3C�  C��C�  C��fC��C�  C�  C��3C��C��3C��3C�  C��C��C�  C�  C�  C��3C�  C��3C�  C�  C��C��C�  C��3C�  C��C��C��C�  C��3C��3C�  C�  C�  C��C��C�  C��3C��3C�  C��C��C��C��C��3C��3C��C��C�  C�  C�  C��C��C��C��C�  C�  C�  C��C��C�  C��C��C��C��C�  C�  C�  C��3C�  C��3C��3C��C�  C�  C��C�  C�  C�  C�  C�  C��3C��3C�  C�  C��3C�  C�  C�  C��C�  C��3C��C��C�  C�  C��C��C�  C��3C��fC�  C�  C��3C��3C�  C��C��C�  C�  C��3C��3C�  C�  C�  C�  C��3D � D  D�fDfD� D  D� D��D� D  D� D��Dy�D��Dy�D  D� D	  D	y�D	��D
� D
��D� D��Dy�Dy�)D�0�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�Q�@��A ��A ��A@��A`��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�G�B =qB=qB=qB=qB =qB(=qB0=qB8=qB@��BH��BP=qBX=qB`=qBh=qBp=qBx=qB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C \C\C\C\C(�C
\C\C\C\C��C\C(�C(�C\C\C\C \C"\C$\C&\C(\C)��C+��C.\C0(�C2\C3��C5��C7��C9��C;��C>\C@(�CB\CC��CF\CH(�CI��CK��CN\CP\CQ��CT\CV\CX\CZ\C\\C^\C`\Cb\Cd\Cf(�Ch\Cj\Cl\Cn(�Cp(�Cr(�Ct\Cv(�Cx\Cy��C{��C}��C��C���C��C��C��C�{C��C��C���C���C��C�{C��C��C�{C��C��C���C�{C���C���C��C�{C�{C��C��C��C���C��C���C��C��C�{C�{C��C���C��C�{C�{C�{C��C���C���C��C��C��C�{C�{C��C���C���C��C�{C�{C�{C�{C���C���C�{C�{C��C��C��C�{C�{C�{C�{C��C��C��C�{C�{C��C�{C�{C�{C�{C��C��C��C���C��C���C���C�{C��C��C�{C��C��C��C��C��C���C���C��C��C���C��C��C��C�{C��C���C�{C�{C��C��C�{C�{C��C���C��C��C��C���C���C��C�{C�{C��C��C���C���C��C��C��C��C���D ��D�D�=D
=D��D�D��D�qD��D�D��D�qD}qD�qD}qD�D��D	�D	}qD	�qD
��D
�qD��D�qD}qDy� D�2�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�^5A�\)A�ffA�n�A�l�A�hsA�n�A�dZA�^5A�K�A��;A��TA�1A�C�A��
AՃA�;dA���A�ĜA���A�&�Aʛ�A��A�&�A�?}AƑhA���A��
A�x�A��Aģ�AüjA�l�A�33A�
=A�=qA��FA�ĜA���A���A��\A��hA�7LA��RA�/A�bNA�ȴA�A�-A�%A�33A�ȴA�ZA���A��`A�|�A�^5A�z�A���A�7LA��A��FA�5?A��\A��/A���A�l�A���A�+A��A�Q�A�?}A��A��A�JA�VA�(�A��A��A�(�A��A��A�&�A��-A��!A���A�G�A���A�?}A�oA��!A�v�A�C�A�{A���A��A�l�A��^A��7A�bA���A���A���A�hsA�&�A���A�-A{Ax5?Av�`AvI�Au�PAt��As��Ar�HAp�RAnbAgl�A_hsA[XAY��AV�yAU�^AT��AS�ASC�AR��ARJAP��AIhsAH  AG��AF�AF�AEl�AD9XAA�A>��A>1'A<JA:9XA8�9A7x�A6�A5��A5
=A4�uA4ZA4(�A4A3��A2z�A0�/A0bA/&�A.A�A-XA,I�A)7LA%%A"��A"�A"�\A!��A �A��AO�A�hAVA$�A�^A+A�A{AhsAbNA{AS�A"�A�AhsA��A�TA�^Ap�AO�A�A+A	|�AVAbA1AZA��AZA  A��AE�A�FA��A�A ff@���@�v�@��@�9X@��\@�%@�I�@��@�-@��`@�33@��@�C�@�+@��y@��@�j@�dZ@�@���@��@��@�z�@�Q�@�A�@��;@�;d@�ȴ@�V@��@�O�@�9X@���@�`B@�7L@�%@��`@ܓu@���@�$�@�Q�@��@ָR@��@�p�@��@Լj@��;@�33@��@�O�@��@��@мj@�9X@ύP@�-@���@���@̣�@��;@�\)@�
=@���@��@ȃ@�9X@�  @Ǿw@�I�@��m@��@�~�@�&�@�-@���@�A�@�|�@�Ft@yzx111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�^5A�\)A�ffA�n�A�l�A�hsA�n�A�dZA�^5A�K�A��;A��TA�1A�C�A��
AՃA�;dA���A�ĜA���A�&�Aʛ�A��A�&�A�?}AƑhA���A��
A�x�A��Aģ�AüjA�l�A�33A�
=A�=qA��FA�ĜA���A���A��\A��hA�7LA��RA�/A�bNA�ȴA�A�-A�%A�33A�ȴA�ZA���A��`A�|�A�^5A�z�A���A�7LA��A��FA�5?A��\A��/A���A�l�A���A�+A��A�Q�A�?}A��A��A�JA�VA�(�A��A��A�(�A��A��A�&�A��-A��!A���A�G�A���A�?}A�oA��!A�v�A�C�A�{A���A��A�l�A��^A��7A�bA���A���A���A�hsA�&�A���A�-A{Ax5?Av�`AvI�Au�PAt��As��Ar�HAp�RAnbAgl�A_hsA[XAY��AV�yAU�^AT��AS�ASC�AR��ARJAP��AIhsAH  AG��AF�AF�AEl�AD9XAA�A>��A>1'A<JA:9XA8�9A7x�A6�A5��A5
=A4�uA4ZA4(�A4A3��A2z�A0�/A0bA/&�A.A�A-XA,I�A)7LA%%A"��A"�A"�\A!��A �A��AO�A�hAVA$�A�^A+A�A{AhsAbNA{AS�A"�A�AhsA��A�TA�^Ap�AO�A�A+A	|�AVAbA1AZA��AZA  A��AE�A�FA��A�A ff@���@�v�@��@�9X@��\@�%@�I�@��@�-@��`@�33@��@�C�@�+@��y@��@�j@�dZ@�@���@��@��@�z�@�Q�@�A�@��;@�;d@�ȴ@�V@��@�O�@�9X@���@�`B@�7L@�%@��`@ܓu@���@�$�@�Q�@��@ָR@��@�p�@��@Լj@��;@�33@��@�O�@��@��@мj@�9X@ύP@�-@���@���@̣�@��;@�\)@�
=@���@��@ȃ@�9X@�  @Ǿw@�I�@��m@��@�~�@�&�@�-@���@�A�@�|�@�Ft@yzx111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B��B��B��B��B��B��B��BǮB��B�wB�}B��B��B��B�}B�jBĜB�5B��BB�B+B49B;dB<jB@�BD�BI�BT�BYB\)B\)BdZBv�B�B�B�B�B�JB��B��B��B��B��B�B�B�B�B��B��B�!BB��B��B��BBĜBǮB�NB�mB�`B�#B��B��BŢB�qB��B�Bo�Bv�Bl�BbNBXBH�B9XB,B�BoB+B��B��B�sBǮB�dB�B��B�+BjBffBcTB`BBZBN�BA�B1'B+B
�B
��B
�B
~�B
{�B
u�B
m�B
@�B
,B
&�B
�B
�B
�B
VB
B	��B	�B	�#B	�B	{�B	bNB	W
B	H�B	A�B	<jB	8RB	5?B	1'B	,B	�B	B��B��B��B�B�B�yB�5B��B��BƨB��B�wB�dB�XB�LB�?B�3B�3B�3B�?B�FB�3B�B�B�B�!B�3B�3B�B��B��B��B��B��B��B��B��B��B��B�uB�bB�hB�bB�\B�DB�B{�By�B{�Bu�Bu�B}�B�+B��B��B��B��B��B�VB�1B�DB�hB�%B}�Bv�Bz�B�1B�PB�DB�=B�7Bx�Bu�Bw�Bu�Br�Bs�Bs�Bu�Bv�Bw�By�B{�B� B�B�B�B�B�B�%B�PB�hB�oB�oB�uB�{B��B��B��B�B�B�B�B�!B�?B�RB�RB�RB�RB�RB�RB�dB�}B�}B��B��BBBBÖBĜBǮBɺBɺBɺB��B��B��B��B��B��B�B�B�B�B�B�#B�HB�NB�TB�`B�B�B�B�B�B�B��B��B��B

�B
�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B��B��B��B��B��B��B��B��B��B��BǮB��B�wB�}B��B��B��B�}B�jBĜB�5B��BB�B+B49B;dB<jB@�BD�BI�BT�BYB\)B\)BdZBv�B�B�B�B�B�JB��B��B��B��B��B�B�B�B�B��B��B�!BB��B��B��BBĜBǮB�NB�mB�`B�#B��B��BŢB�qB��B�Bo�Bv�Bl�BbNBXBH�B9XB,B�BoB+B��B��B�sBǮB�dB�B��B�+BjBffBcTB`BBZBN�BA�B1'B+B
�B
��B
�B
~�B
{�B
u�B
m�B
@�B
,B
&�B
�B
�B
�B
VB
B	��B	�B	�#B	�B	{�B	bNB	W
B	H�B	A�B	<jB	8RB	5?B	1'B	,B	�B	B��B��B��B�B�B�yB�5B��B��BƨB��B�wB�dB�XB�LB�?B�3B�3B�3B�?B�FB�3B�B�B�B�!B�3B�3B�B��B��B��B��B��B��B��B��B��B��B�uB�bB�hB�bB�\B�DB�B{�By�B{�Bu�Bu�B}�B�+B��B��B��B��B��B�VB�1B�DB�hB�%B}�Bv�Bz�B�1B�PB�DB�=B�7Bx�Bu�Bw�Bu�Br�Bs�Bs�Bu�Bv�Bw�By�B{�B� B�B�B�B�B�B�%B�PB�hB�oB�oB�uB�{B��B��B��B�B�B�B�B�!B�?B�RB�RB�RB�RB�RB�RB�dB�}B�}B��B��BBBBÖBĜBǮBɺBɺBɺB��B��B��B��B��B��B�B�B�B�B�B�#B�HB�NB�TB�`B�B�B�B�B�B�B��B��B��B

�B
�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.06 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191704                              AO  ARCAADJP                                                                    20181005191704    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191704  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191704  QCF$                G�O�G�O�G�O�8000            