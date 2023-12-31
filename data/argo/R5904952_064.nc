CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  M   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:19Z creation      
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
resolution        =���   axis      Z        4  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 P  >�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     4  ?�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 P  E(   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     4  Fx   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     4  K�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 P  P�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     4  R0   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 P  Wd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     4  X�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     4  ]�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 P  c   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     4  dl   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 P  i�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     4  j�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  p$   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    pT   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    sT   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    vT   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  yT   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    y�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    y�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    y�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    y�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  y�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    y�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    y�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    y�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         y�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         y�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        y�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    z Argo profile    3.1 1.2 19500101000000  20181005190519  20181005190519  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               @A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @׿a�j�1   @׿a���&@1]p��
=�c�ě��T1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      @A   A   A   @333@�  @�  @���A   A>ffA`  A�  A�  A�  A�  A�  A���A�  A�  B ffBffB  B  B   B(  B0  B7��B@  BH  BP  BXffB`  Bh  Bp  Bx  B�  B�33B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cm�fCp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C��3C��3C�  C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D��D� D  Dy�D��D� D  D� D  D� D  D� D	  D	� D
  D
� D  D�fD  D� D  D� D  D� DfD�fDfD� D  D� D��D� DfD�fD  Dy�D  D� DfD� D��D� D  D� D  D� D  D�fD  D� D  D� D��Dy�D��Dy�D  D� D   D � D ��D!y�D"  D"� D#  D#� D$  D$� D%  D%y�D&  D&� D'  D'� D(  D(�fD)  D)y�D*  D*� D+  D+�fD,fD,�fD-  D-� D.fD.� D/  D/� D0  D0�fD1fD1��D1�f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @@  @�ff@�ffA��A#33AA��Ac33A���A���A���A���A���A�fgAᙚA�B33B	33B��B��B ��B(��B0��B8fgB@��BH��BP��BY33B`��Bh��Bp��Bx��B�ffB���B�ffB�ffB�ffB�ffB�ffB���B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffC33C33C33C33C33C33C33C 33C"33C$33C&33C(33C*33C,33C.33C033C233C433C633C833C:33C<33C>33C@33CB33CD33CF33CH33CJ33CL33CN33CP33CR33CT33CV33CX33CZ33C\33C^33C`33Cb33Cd33Cf33Ch33Cj33Cl33Cn�Cp33Cr33Ct33Cv33Cx33Cz33C|33C~33C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&gC��C��C��C��C��C��C��C��C�&gC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&gC��C��C��C��C��C��C��C��C��C��C��C�&gC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&gC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&gC��C��C��C��C��C��C��C��D �D ��D�D��D�D��DgD��D�D�gDgD��D�D��D�D��D�D��D	�D	��D
�D
��D�D�3D�D��D�D��D�D��D3D�3D3D��D�D��DgD��D3D�3D�D�gD�D��D3D��DgD��D�D��D�D��D�D�3D�D��D�D��DgD�gDgD�gD�D��D �D ��D!gD!�gD"�D"��D#�D#��D$�D$��D%�D%�gD&�D&��D'�D'��D(�D(�3D)�D)�gD*�D*��D+�D+�3D,3D,�3D-�D-��D.3D.��D/�D/��D0�D0�3D13D1�gD1�3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aه+Aى7Aى7Aه+Aه+Aه+Aه+Aه+Aى7Aى7Aى7AٍPAٍPAُ\AّhAّhAّhAٓuAٓuAُ\AّhAّhA��A�A��AӍPA� �A��TA�Q�A�p�A�-Aԡ�A���A��A�v�A�r�Aѡ�A�1'A�M�A��TAϴ9Aω7A�;dA�bA���A�5?A���A� �A�9XA�;dA��A��FA�ȴA�M�A�t�A���A�A��!A��A��A�ȴA�ĜA�^5A�\)A��A���A���A��A�r�A��A�oA���A��hA�?}A�A��hA��FA��A�hsA�/A��jA�bA�/A��/A���A�p�A��;A��A�p�A��uA�(�A|�Ax��Aw��Av�+Au`BAs\)Ar$�Al{Af$�AcK�A_�wA^~�A\��AY��AXffAV��ARbNAO�ANA�AMC�AL{AKG�AJ(�AHr�AFz�ADM�AB�\A?/A;��A9��A8ffA7|�A6�A3;dA/�;A-�#A,��A+%A*��A*�jA(�9A(9XA'��A& �A#�A"VAK�AQ�A��A�A�A1'AS�A�AdZA�A�uA��AXA\)AVA{A�AO�A^5A�A��AbNA��A%Az�A�yA�#A��A�A�wAx�A&�A
�uA
1A	"�A-A�#AA�An�AA5?A/A �A �D@��`@�  @�V@��u@�F@�;d@�!@�{@��#@�@�O�@���@��@�9X@@�M�@���@��@��@��@�x�@��@���@�@���@�^@�-@�^@�@�$�@�=q@�-@�$�@��@�`B@���@�7L@�o@�V@�x�@��@�r�@���@�C�@ޏ\@�-@��@݁@�`B@�Q�@�S�@�~�@��@���@��/@���@Ԭ@��`@�{@Լj@�z�@�  @�"�@�5?@�x�@���@� �@�ƨ@��H@Χ�@��#@͑h@�p�@��@��@̣�@�S�@�~�@�ff@�~�@�E�@ɺ^@�O�@���@���@�Ĝ@ȼj@ȴ9@�j@�|�@�;d@��y@Ə\@��#@î@��#@��@�7L@�1'@�"�@��@��R@��!@���@��@�&�@�Q�@���@�;d@��@�Z@�I�@�b@��@�7L@��@��u@�1@���@�;d@�33@��@��\@�M�@�^5@�M�@�=q@�-@�V@�ff@�ff@�-@���@��T@��^@���@�V@���@�@���@��u@�9X@��@�
=@�M�@���@�7L@��`@���@�j@��D111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aه+Aى7Aى7Aه+Aه+Aه+Aه+Aه+Aى7Aى7Aى7AٍPAٍPAُ\AّhAّhAّhAٓuAٓuAُ\AّhAّhA��A�A��AӍPA� �A��TA�Q�A�p�A�-Aԡ�A���A��A�v�A�r�Aѡ�A�1'A�M�A��TAϴ9Aω7A�;dA�bA���A�5?A���A� �A�9XA�;dA��A��FA�ȴA�M�A�t�A���A�A��!A��A��A�ȴA�ĜA�^5A�\)A��A���A���A��A�r�A��A�oA���A��hA�?}A�A��hA��FA��A�hsA�/A��jA�bA�/A��/A���A�p�A��;A��A�p�A��uA�(�A|�Ax��Aw��Av�+Au`BAs\)Ar$�Al{Af$�AcK�A_�wA^~�A\��AY��AXffAV��ARbNAO�ANA�AMC�AL{AKG�AJ(�AHr�AFz�ADM�AB�\A?/A;��A9��A8ffA7|�A6�A3;dA/�;A-�#A,��A+%A*��A*�jA(�9A(9XA'��A& �A#�A"VAK�AQ�A��A�A�A1'AS�A�AdZA�A�uA��AXA\)AVA{A�AO�A^5A�A��AbNA��A%Az�A�yA�#A��A�A�wAx�A&�A
�uA
1A	"�A-A�#AA�An�AA5?A/A �A �D@��`@�  @�V@��u@�F@�;d@�!@�{@��#@�@�O�@���@��@�9X@@�M�@���@��@��@��@�x�@��@���@�@���@�^@�-@�^@�@�$�@�=q@�-@�$�@��@�`B@���@�7L@�o@�V@�x�@��@�r�@���@�C�@ޏ\@�-@��@݁@�`B@�Q�@�S�@�~�@��@���@��/@���@Ԭ@��`@�{@Լj@�z�@�  @�"�@�5?@�x�@���@� �@�ƨ@��H@Χ�@��#@͑h@�p�@��@��@̣�@�S�@�~�@�ff@�~�@�E�@ɺ^@�O�@���@���@�Ĝ@ȼj@ȴ9@�j@�|�@�;d@��y@Ə\@��#@î@��#@��@�7L@�1'@�"�@��@��R@��!@���@��@�&�@�Q�@���@�;d@��@�Z@�I�@�b@��@�7L@��@��u@�1@���@�;d@�33@��@��\@�M�@�^5@�M�@�=q@�-@�V@�ff@�ff@�-@���@��T@��^@���@�V@���@�@���@��u@�9X@��@�
=@�M�@���@�7L@��`@���@�j@��D111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
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
��B
��B
��B
��B
n�B
jB
w�B
w�B
��B
��B
�B
��B
��B
�B
��BhB�B/B6FB7LB8RB<jBA�BF�BP�B_;BffBm�Bt�B� B�\B?}B=qB:^B6FB,B%�B#�B!�B�B�B{BVB+BJB\B+B1'B0!B&�B �B
=B��B�B��B�B��B��B�Bn�B_;BL�BC�B9XB�B
�B
�ZB
�/B
�}B
�=B
n�B
N�B
B�B
�B
	7B
B	��B	�B	�5B	��B	�B	�%B	s�B	e`B	_;B	XB	J�B	E�B	>wB	/B	!�B	�B	oB	
=B	B��B��B�B�BB�BƨB�^B�XB�wB�wB�^B�3B�!B�B�!B�-B�'B�!B�^B�wB�qB��B�XB�9B�'B�FB�XB�FB��BÖB�jB�3B�!BĜBĜBǮBɺB��B��B�
B�5B�B	+B	�B	�B	�B	oB	{B	&�B	$�B	�B	"�B	#�B	#�B	"�B	 �B	�B	{B	JB	+B��B	�B	�B	�B	 �B	�B	bB	%B��B�ZB�B�B�B�B�
B�B�
B�
B�B�B�B�B��B��B��B�B�)B�ZB�mB�B�B��B��B��B��B	B	B	
=B	oB	�B	�B	�B	�B	�B	�B	%�B	&�B	,B	2-B	49B	6FB	1'B	+B	+B	0!B	:^B	<jB	;dB	9XB	9XB	;dB	6FB	33B	2-B	9XB	C�B	N�B	^5B	_;B	dZB	iyB	jB	jB	l�B	m�B	p�B	r�B	s�B	s�B	s�B	u�B	y�B	y�B	y�B	x�B	v�B	y�B	|�B	~�B	� B	� B	�B	�B	�B	�B	�B	�B	�1B	�=B	�=B	�7B	�1B	�%B	�B	{�B	{�B	�B	�B	~�B	~�B	~�B	}�B	}�B	}�B	~�B	~�B	�B	�B	�JB	�oB	�uB	�uB	�oB	�bB	�bB	�\B	�\B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�-B	�RB	�LB	�9B	�-B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   B
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
��B
��B
��B
��B
n�B
jB
w�B
w�B
��B
��B
�B
��B
��B
�B
��BhB�B/B6FB7LB8RB<jBA�BF�BP�B_;BffBm�Bt�B� B�\B?}B=qB:^B6FB,B%�B#�B!�B�B�B{BVB+BJB\B+B1'B0!B&�B �B
=B��B�B��B�B��B��B�Bn�B_;BL�BC�B9XB�B
�B
�ZB
�/B
�}B
�=B
n�B
N�B
B�B
�B
	7B
B	��B	�B	�5B	��B	�B	�%B	s�B	e`B	_;B	XB	J�B	E�B	>wB	/B	!�B	�B	oB	
=B	B��B��B�B�BB�BƨB�^B�XB�wB�wB�^B�3B�!B�B�!B�-B�'B�!B�^B�wB�qB��B�XB�9B�'B�FB�XB�FB��BÖB�jB�3B�!BĜBĜBǮBɺB��B��B�
B�5B�B	+B	�B	�B	�B	oB	{B	&�B	$�B	�B	"�B	#�B	#�B	"�B	 �B	�B	{B	JB	+B��B	�B	�B	�B	 �B	�B	bB	%B��B�ZB�B�B�B�B�
B�B�
B�
B�B�B�B�B��B��B��B�B�)B�ZB�mB�B�B��B��B��B��B	B	B	
=B	oB	�B	�B	�B	�B	�B	�B	%�B	&�B	,B	2-B	49B	6FB	1'B	+B	+B	0!B	:^B	<jB	;dB	9XB	9XB	;dB	6FB	33B	2-B	9XB	C�B	N�B	^5B	_;B	dZB	iyB	jB	jB	l�B	m�B	p�B	r�B	s�B	s�B	s�B	u�B	y�B	y�B	y�B	x�B	v�B	y�B	|�B	~�B	� B	� B	�B	�B	�B	�B	�B	�B	�1B	�=B	�=B	�7B	�1B	�%B	�B	{�B	{�B	�B	�B	~�B	~�B	~�B	}�B	}�B	}�B	~�B	~�B	�B	�B	�JB	�oB	�uB	�uB	�oB	�bB	�bB	�\B	�\B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�-B	�RB	�LB	�9B	�-B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.20 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190519                              AO  ARCAADJP                                                                    20181005190519    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190519  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190519  QCF$                G�O�G�O�G�O�8000            