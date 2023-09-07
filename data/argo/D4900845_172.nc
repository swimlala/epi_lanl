CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-05-18T15:24:28Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  A�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Ht   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  J    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  P�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Wx   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Y$   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  _�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  a|   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h(   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  n�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w,   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  x�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �0   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �@   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �D   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �T   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �X   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �\   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �`Argo profile    3.1 1.2 19500101000000  20180518152428  20181001131238  4900845 US ARGO PROJECT                                                 BRECK OWENS                                                     PRES            TEMP            PSAL               �A   AO  2586                            2C  D   SOLO_W                          0760                            1.1                             851 @�ZWs��1   @�ZWs��@:����l��S@A�7K�1   GPS     Primary sampling: averaged []                                                                                                                                                                                                                                      A   A   A   @   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D0@ D1� D2� D4  D5@ D6� D7� D9  D:@ D;� D<� D>  D?@ D@� DA� DC  DD@ DE� DF� DH  DI@ DJ� DK� DM  DN@ DO� DP� DR  DS@ DT� DU� DW  DX@ DY� DZ� D\  D]@ D^� D_� Da  Db@ Dc� Dd� Df  Dg@ Dh� Di� Dk  Dl@ Dm� Dn� Dp  Dq@ Dr� Ds� Du  Dv@ Dw� Dx� Dz  D{@ D|� D}� D  D�  D�� D�` D�  D�� D�@ D�� D�� D�  D�� D�` D�  D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D0@ D1� D2� D4  D5@ D6� D7� D9  D:@ D;� D<� D>  D?@ D@� DA� DC  DD@ DE� DF� DH  DI@ DJ� DK� DM  DN@ DO� DP� DR  DS@ DT� DU� DW  DX@ DY� DZ� D\  D]@ D^� D_� Da  Db@ Dc� Dd� Df  Dg@ Dh� Di� Dk  Dl@ Dm� Dn� Dp  Dq@ Dr� Ds� Du  Dv@ Dw� Dx� Dz  D{@ D|� D}� D  D�  D�� D�` D�  D�� D�@ D�� D�� D�  D�� D�` D�  D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�E�A�C�A�Q�A�XA�\)A�dZA�jA�hsA�ffA�VA���AԸRAԏ\A�S�A�"�A�  Aӟ�A�bA��A��#A�ĜAң�A�A�A�O�A�?}AН�A�1'A��HA�~�A��mA�ZA�/A�A��yA�v�A�"�A�jA�VA�=qAǏ\A�t�A�(�A�%A���A�Q�A�33A¬A��/A�A��^A��wA�  A�ĜA��+A�5?A�1'A�~�A��A�/A��A�I�A�E�A���A�jA�JA���A��A��!A��
A��A�jA��mA�t�A���A�O�A���A��A���A��A�%A�VA��PA�(�A�l�A��A�ȴA��^A��-A��A��PA�O�A�-A���A��A��A���A���A�-A�ƨA�`BA�ZA�+A�p�A��A�1A��9A�p�A���A�&�A��RA��A�\)A�7LA�oA�A��HA��7A�E�A���A���A��RA��DA�ffA�
=A��;A�\)A�JA���A��`A��#A�ĜA���A�v�A�1'A��^A�?}A�JA��mA��uA�-A��9A�l�A�bA���A��\A�jA�S�A�/A�ƨA�A�A��A�ƨA���A�bNA�-A���A�ZA�{A���A��#A��7A�O�A�bA��A�VA��A�%A��A��A��!A��PA�t�A�l�A�M�A��A���A��A�ƨA��RA��FA��wA��^A��-A��!A��!A��A�&�A��yA��!A�dZA�M�A�=qA�1'A�{A��A���A���A�;dA�A��;A�ƨA��jA��A��DA�hsA�7LA�A���A�z�A�I�A�?}A�7LA�A�A���A�VA�1A��A�ȴA�E�A��^A�\)A�A�A�/A��TA�ĜA��PA�G�A��#A��\A�VA� �A�ȴA���A���A���A� �A�t�A���A�|�A�`BA�;dA�(�A���A��^A��7A�E�A���A�S�A�ĜA��-A�?}AS�A}�A}&�A{O�Ay�wAx�uAvJAt�9At�At�At��At�DAtM�As%Aq�Ao�mAnA�Am��Am|�AmC�Al�yAl=qAj�/Ah��Ag��Af��AeK�Ad�DAcC�Aa&�A_S�A^-A]�PA\jAZz�AY�AX1'AVQ�AT�AS%AQ?}AO��AM��ALJAJ^5AHĜAG�hAE�mAC��AB�yAB�AA�;AA��A@�yA@I�A?�A=�A=hsA=oA<��A;�;A;`BA:��A:bA9�-A9G�A8Q�A733A4��A3+A2�!A2�\A2bNA2(�A1��A05?A.��A-;dA,M�A+?}A*��A)��A'l�A&1A$��A#��A#7LA"�HA!dZA ��A ��A�FA;dAȴA�A�+A�AZA1A��A/AĜA�wA��A�;A�DA�A�AA�A�!AE�AbAO�A�A�uA�!A
�!A	?}A�;A��A^5A�wAoA��A��A�AV@���A Q�@��;@�?}@�C�@�=q@���@�M�@�5?@�9@�hs@�?}@�@�Q�@�J@�V@Ұ!@�9X@���@��@�+@��m@�\)@�5?@��R@���@���@��@�Q�@��@��`@�
=@�Ĝ@��@��H@�hs@��F@���@��T@��@��@���@�A�@�  @��@��;@��;@���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�E�A�C�A�Q�A�XA�\)A�dZA�jA�hsA�ffA�VA���AԸRAԏ\A�S�A�"�A�  Aӟ�A�bA��A��#A�ĜAң�A�A�A�O�A�?}AН�A�1'A��HA�~�A��mA�ZA�/A�A��yA�v�A�"�A�jA�VA�=qAǏ\A�t�A�(�A�%A���A�Q�A�33A¬A��/A�A��^A��wA�  A�ĜA��+A�5?A�1'A�~�A��A�/A��A�I�A�E�A���A�jA�JA���A��A��!A��
A��A�jA��mA�t�A���A�O�A���A��A���A��A�%A�VA��PA�(�A�l�A��A�ȴA��^A��-A��A��PA�O�A�-A���A��A��A���A���A�-A�ƨA�`BA�ZA�+A�p�A��A�1A��9A�p�A���A�&�A��RA��A�\)A�7LA�oA�A��HA��7A�E�A���A���A��RA��DA�ffA�
=A��;A�\)A�JA���A��`A��#A�ĜA���A�v�A�1'A��^A�?}A�JA��mA��uA�-A��9A�l�A�bA���A��\A�jA�S�A�/A�ƨA�A�A��A�ƨA���A�bNA�-A���A�ZA�{A���A��#A��7A�O�A�bA��A�VA��A�%A��A��A��!A��PA�t�A�l�A�M�A��A���A��A�ƨA��RA��FA��wA��^A��-A��!A��!A��A�&�A��yA��!A�dZA�M�A�=qA�1'A�{A��A���A���A�;dA�A��;A�ƨA��jA��A��DA�hsA�7LA�A���A�z�A�I�A�?}A�7LA�A�A���A�VA�1A��A�ȴA�E�A��^A�\)A�A�A�/A��TA�ĜA��PA�G�A��#A��\A�VA� �A�ȴA���A���A���A� �A�t�A���A�|�A�`BA�;dA�(�A���A��^A��7A�E�A���A�S�A�ĜA��-A�?}AS�A}�A}&�A{O�Ay�wAx�uAvJAt�9At�At�At��At�DAtM�As%Aq�Ao�mAnA�Am��Am|�AmC�Al�yAl=qAj�/Ah��Ag��Af��AeK�Ad�DAcC�Aa&�A_S�A^-A]�PA\jAZz�AY�AX1'AVQ�AT�AS%AQ?}AO��AM��ALJAJ^5AHĜAG�hAE�mAC��AB�yAB�AA�;AA��A@�yA@I�A?�A=�A=hsA=oA<��A;�;A;`BA:��A:bA9�-A9G�A8Q�A733A4��A3+A2�!A2�\A2bNA2(�A1��A05?A.��A-;dA,M�A+?}A*��A)��A'l�A&1A$��A#��A#7LA"�HA!dZA ��A ��A�FA;dAȴA�A�+A�AZA1A��A/AĜA�wA��A�;A�DA�A�AA�A�!AE�AbAO�A�A�uA�!A
�!A	?}A�;A��A^5A�wAoA��A��A�AV@���A Q�@��;@�?}@�C�@�=q@���@�M�@�5?@�9@�hs@�?}@�@�Q�@�J@�V@Ұ!@�9X@���@��@�+@��m@�\)@�5?@��R@���@���@��@�Q�@��@��`@�
=@�Ĝ@��@��H@�hs@��F@���@��T@��@��@���@�A�@�  @��@��;@��;@���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBĜBĜBĜBĜBĜBĜBĜBĜBÖBĜB��B��B��B��B��B�
B�/B�fB�yB�B�B�B��BbBhB!�B.B:^BF�Bo�B�VB�PB�\B�\B�{B�qBɺB��B��B��B�)B�/B�/B�B�B�B�B��B��B��BB\BhBoBbBhB�BuBuB�B �B,B33B2-B1'B.B-B-B49B.B%�B&�B%�B$�B#�B!�B�B�B�B�BuBDB1BBB  B��B��B��B��B��B��B��B�B�B�B�B�B�B�B��B��B��BB�B�B�fB��BȴBŢBB��B�}B�qB�qB�dB�RB�?B�'B�B��B��B��B��B��B��B��B��B��B��B��B��B�{B�oB�bB�VB�PB�DB�7B�+B�B�B|�B{�Bx�Bu�Bt�Br�Bo�BjBgmBe`BcTB_;B]/BYBT�BR�BP�BN�BK�BI�BE�BB�B>wB<jB;dB:^B8RB7LB5?B49B33B2-B2-B0!B/B.B-B-B-B,B,B,B+B)�B#�B �B�B�B�B�B{BoB\BJB	7BB��B��B��B��B��B�B�B�B�mB�TB�)B�B�B�B��B��BȴB��B�dB�LB�3B��B��B�oB�bB�PB�+B�B~�Bw�Bn�BgmBbNB\)BS�BM�B@�B6FB,B�BbB+BB  B��B�B�/B�B��BǮB�wB�!B�B��B��B�+B{�BjBVBI�B2-B!�B!�B!�B �B�B�BbB��B�B�BB�B�
B��B��BȴB�^B��B�{B�+Bu�BjB`BBO�BB�B:^B2-B'�BoBB��B�mB��BɺB�dB�B��B�PB}�Bn�B`BBR�BB�B;dB8RB2-B/B)�B#�B�BhBDB+BB��B��B��B�B�B�yB�;B�B��B�LB�3B�'B�B�B��B��B�bB�B{�Bt�Bn�BffBZBQ�BK�BD�BA�B>wB6FB2-B/B+B'�B$�B%�B#�B#�B!�B �B�B�B�BVB  B�B�B�B�B�yB�BB�5B�5B�B��B��BƨB�B��B�oB�JB�\B�bB�\B�+B�+B��B�oB�B�oB�uB�PB�1B�B~�B� Bw�Bl�Be`B\)BXBW
BQ�BK�BJ�BL�BE�B@�B>wB<jB<jB:^B9XB9XB9XB8RB7LB49B1'B.B+B&�B#�B!�B�B�B�B�B�B�B�B{B{B{B{B{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�/B�)B�/B�/B�/B�/B�/B�/B�)B�;B�TB�ZB�sB�B�B�B��B��BBB%B
=BuB(�B-B<jBG�BT�BffB�\B��B��B��B��B�RB�B�NB�TB�fB�B��B��B��BDB	7B
=B\BhB{B�B �B(�B+B-B.B2-B2-B0!B1'B49B9XBE�BM�BL�BK�BI�BG�BI�BO�BI�BA�BA�BA�B?}B?}B<jB:^B9XB8RB7LB.B%�B#�B�B�B�B�B�B�B{BuBhBhBDB1B1BDBJBPBJBVBoBbB �B1B1BB�B�TB�;B�)B�B�B�B�B��B��B��B��BĜBB��B�}B�jB�dB�LB�3B�-B�'B�'B�!B�!B�B�B�B��B��B��B��B��B��B��B��B��B�hB�VB�VB�PB�=B�B�B~�B|�Bx�Bw�Br�Bn�Bk�BiyBhsBe`BcTB`BB\)BXBT�BS�BR�BP�BO�BM�BL�BK�BK�BJ�BH�BG�BF�BE�BD�BE�BD�BD�BD�BD�BC�B=qB:^B5?B/B/B.B-B+B'�B$�B#�B�B�BuBhBbBVBJB	7BBB��B��B�B�B�B�B�ZB�NB�#B��B��B��BB�LB�B��B��B��B��B��B�oB�1B�B{�Bu�Bm�BiyB[#BP�BG�B9XB)�B�B�B�B�B\B��B�B�B�NB�BȴBǮB�}B�-B��B��B�%Bq�BhsBM�B:^B9XB9XB9XB8RB6FB,B�BJB��B�B�B�B�yB�ZB�B��B�B��B�\B�%B}�Bl�B]/BS�BL�BD�B.B�B{BB�B�fB�BȴB�3B��B��B�7B|�Bp�B\)BS�BQ�BJ�BH�BC�B>wB7LB)�B#�B�B�B�BuBbB	7BBB��B�B�/B��B��BȴBŢBĜBÖB�FB�B��B��B�VB�7B�Bu�Bl�BffB]/BZBZBN�BI�BI�BC�B@�B<jB=qB;dB;dB9XB8RB5?B2-B0!B'�B�B
=BBB+BB��B��B��B�B�yB�B�BBǮB�FB�B��B��B��B��B��B��B�?B�B��B��B�B��B��B��B��B��B�oB�%B~�Bu�Bo�Bo�Bk�BffBcTBe`BaHBYBW
BS�BS�BS�BP�BP�BP�BO�BN�BL�BI�BF�BD�B?}B;dB9XB7LB6FB5?B2-B0!B.B-B+B+B+B+B+1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�<#�<#�<#ا<#��<#ٛ<#�<#��<#�Q<(7�<$/<$U�<$��<$}�<$><<&/<)�<$�<#�g<#�a<$2G<&�a<2P�<#��<*�<&��<%��<'uq<NN�<G�<$Z�<$o�<$
<)7,<q�<,<�<#��<$'<,�X<7��<%}�<$��<>n�<E݃<$MO<)[h</�N<0�><%�b<5�H<-͝<$��<%�<&Gi<7� <Q��<*r�</w�<96�</��<#�U<%�J<)?0<&�^<'T�<){�<'�.<1ߵ<.3�<,��<)�6<(�<,A�<(�)<-c�<)?0<(>�<.u<5�?<b��<)?0<'|<.u<&U"<%��<#�<#�<#�o<$/%<$�-<$MO<&�*<27a<%B�<#؄<#�<(I�<'><&�<#ޫ<$�w<-ݨ<'�<4�<'G�<%b�<+��<-i<(�<$�h<$c�<$H�<$C�<#��<$=<&?><%�#<-D�<+><%v�<$� <$]h<&n4<$�j<)�<%�b<#�H<#�(<#��<$<<$T�<$_�<%��<(}�<(��<$�w<$^�<&7"<'hA<(��<%�L<&�^<%S�<%4L<$MO<$�<$c�<'n�<)c�<&'<$t <$�<$��<% �<)�<%��<%m�<$)
<$"2<%�<%�<%>�<'�<&h�<$��<$v<#��<$F<$m,<$@|<$	�<#��<$+<$��<$/%<#��<$R'<#�<#�<#�r<#�<#�<#�X<#�I<$�Q<&c<%I<%�<%��<$�<#�"<#�M<$Z<$m,<$<<$��<'<%s<$I�<$.<#�<#��<$6�<$@|<$��<$��<$��<&
(<$��<#�<#�<$�w<%,#<$2G<%�!<%��<$�<$�<)�L<*\�<&�*<$=<#�N<%��<$:�<$�!<%�l<'��<%�`<%�<$�.<&W�<$�k<+Z�<'�<(ܠ<-��<,�r<&�^<$><<$R'<$�<+'�<1��<%I<%�M<(��<)7,<*9�<$B�<(T�<+��<.T�<(}�<5|�<1��<- 6<D�a<.>�<#��<#�<#׎<#�5<$XX<,�<0�t<2�a<2�<%��<$r<$'<$��<&�8</`�<:d�<,$;<);-<0�A<(!�<.�x<<��<7��<,�~<&�3<,��<8݆</��<*�f<8Q|<?+h<,A�<6b<4X�<;�?<3��<57�<3x<-�L<5u�<@��<&�<$j|<'x�<$�<&�<&�R<-m<, <%��<$�j<$.<*��<%��<%:<(��<$��<%04<)�<,�<C	�<4��<%b�<#��<$�<$@|<&O�</��<3�r</�"<*
c<*��<&�A<+��<?�<19D<.��<+v�<%.+<$��<07w<%\\<$f<+��<%��<% <#�H<#��<#�$<#�a<#�Q<$�<$�<$	�<$��<'�O<'��<%�j<$�<$A�<%m�<&�^<$�<#�<$i&<'hA<$��<'��<(<&1�<&�<%<$$<$F9<$XX<&�<%'<%�J<&1�<*��<#��<#�N<%��<$�<$%<%��<$\"<(F.<,d}<'4l<(�T<(j<$T�<%I<*�</}~<%��<&��<1�<&��<'*<#�W<$7�<'n�<$#(<#�o<#�<$�<$Gd<%I<$��<%m�<'��<%k�<$��<$��<$<<$<<$��<$��<$Gd<$<<#�8<#ף<#�i<#�<#׎<#�PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Conductivity Thermal Mass (CTM), Johnson et al., 2007, JAOT; PSAL_ADJ = CTM_ADJ_PSAL + dS, dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s, rise rate = 10 cm/s with error equal to the adjustment; OW: r =1.0006(+/-0), vertically averaged dS =0.027(+/-0.001)                                                                                                              No significant pressure drift detected                                                                                                                                                                                                                          No significant temperature drift detected                                                                                                                                                                                                                       Significant salinity drift present, OW fit adopted: fit for cycles 0 to 197.  Map Scales:[x=4,2; y=2,1].  The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                            201810010000002018100100000020181001000000  AO  ARGQQCPL                                                                    20180518152428  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20180518152428  QCF$                G�O�G�O�G�O�0               WHOIARSQWHQCV0.5                                                                20181001000000  QC                  G�O�G�O�G�O�                WHOIARSQ CTMV1.0                                                                20181001000000  IP                  G�O�G�O�G�O�                WHOIARCAOW  V1.1ARGO_for_DMQC_2017V03                                           20181001000000  IP                  G�O�G�O�G�O�                