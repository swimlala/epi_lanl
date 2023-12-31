CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  ]   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:18Z creation      
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
resolution        =���   axis      Z        t  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 `  >�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     t  @D   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 `  E�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     t  G   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     t  L�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 `  R    TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     t  S`   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 `  X�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     t  Z4   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     t  _�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 `  e   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     t  f|   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 `  k�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     t  mP   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  r�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    r�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    u�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    x�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  {�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    |    HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    |$   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    |(   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    |,   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  |0   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    |p   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    |�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    |�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         |�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         |�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        |�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    |�Argo profile    3.1 1.2 19500101000000  20181005190518  20181005190518  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               :A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @׽�E�1   @׽�5�<@1H1&�x��c��t�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      :A   A   A   @�  @�  A��A!��A@  A^ffA�  A���A�  A�  A�  A���A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BW��B`  BhffBp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�33C   C�fC  C  C�C
�C  C  C�fC  C  C  C  C  C  C  C   C"  C$  C&�C(  C*  C,  C.  C0  C2  C3�fC6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cm�fCp  Cr  Ct  Cv�Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C��C��C��C��C�  C��3C�  C��C��C�  C�  C�  C�  C�  D   D � D  Dy�D��D� DfD�fD  D� D��D� DfD� D  D� D  D� D	  D	� D
  D
� D
��D� DfD�fDfD�fDfD� D  D� DfD� D  D� D  Dy�D  D� D  D� DfD� D��D� DfD� D  Dy�D��D� D��Dy�D  Dy�D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#y�D#��D$� D%fD%�fD&  D&y�D'  D'�fD(  D(� D)  D)� D*fD*� D+  D+� D,  D,� D-  D-�fDy�fD�5D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��A�]A&�]AD��Ac\)A�z�A�G�A�z�A�z�A�z�A�G�A�G�A�z�B=qB	=qB=qB=qB!=qB)=qB1=qB9=qBA=qBI=qBQ=qBX�Ba=qBi��Bq=qBy=qB���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BĞ�B�k�B̞�BО�BԞ�B؞�B���B���B䞸B螸B잸B�B���B���B���C O\C5�CO\CO\Ch�C
h�CO\CO\C5�CO\CO\CO\CO\CO\CO\CO\C O\C"O\C$O\C&h�C(O\C*O\C,O\C.O\C0O\C2O\C45�C6O\C8O\C:O\C<O\C>O\C@O\CBO\CDO\CFO\CHO\CJO\CLO\CNO\CPO\CRO\CTO\CVO\CXO\CZO\C\O\C^O\C`O\CbO\CdO\CfO\ChO\CjO\ClO\Cn5�CpO\CrO\CtO\Cvh�CxO\CzO\C|O\C~O\C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�4{C�4{C�'�C�'�C��C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�4{C�'�C��C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C��C��C�'�C�'�C�'�C�'�C��C�'�C�'�C�'�C�'�C�'�C��C��C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C��C��C�'�C��C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�4{C�'�C�'�C�'�C�'�C�'�C�4{C�4{C�4{C�4{C�4{C�'�C��C�'�C�4{C�4{C�'�C�'�C�'�C�'�C�'�D �D ��D�D�qDqD��D=D�=D�D��DqD��D=D��D�D��D�D��D	�D	��D
�D
��DqD��D=D�=D=D�=D=D��D�D��D=D��D�D��D�D�qD�D��D�D��D=D��DqD��D=D��D�D�qDqD��DqD�qD�D�qD�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#�qD$qD$��D%=D%�=D&�D&�qD'�D'�=D(�D(��D)�D)��D*=D*��D+�D+��D,�D,��D-�D-�=Dy�=D�?D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A���A�ƨA���A���A�$�A�E�A�bNA�r�A�|�AڅAړuAڬAڶFAڥ�Aڟ�Aڛ�A�v�A�1'A��A�  A؝�A��HAדuAו�A�5?A֝�A�p�A�ĜA�O�Aԕ�A��AсA� �A���A�\)A��Aω7A�7LA��;A�(�A���A�Aͣ�A͑hA�r�A� �A��/A��A���A�-A�bNA�1'A��#A��A��A���A�ffA��A�XA�A��A��wA��\A���A�l�A��FA�bNA�I�A�(�A�VA��^A�A���A�-A��hA��A��PA�|�A��\A�;dA�ȴA��7A��wA�bA�A�
=A�n�A�9XA��RA��FA�ƨA�x�A�XA�=qA��A��jA�/A��PA��A�%A�XA��TA��-A�/A�K�A�x�A�n�A�1A}C�A{�Av�Arv�Am�Ai�PAf�uAcp�A`�yA^��A]�PA\ȴA[AY�#AX��AVJASC�APz�AOx�AN9XALbNAJ��AH�AF �AB�!A>�!A<��A:�/A:z�A8~�A6�A6�A5��A3�A/�FA-��A,I�A*��A)�
A&�yA$�HA#�wA"��A" �A ��A $�A/A��AhsA�jAI�A�mAA��A�mA��Ap�A��Ax�A%AA�A1AffAp�A=qA��A%A�DAA�At�AVA;dA�A
�A
��A
�`A
�yA
�jA	�;A	+At�A5?A�9A
�uA	�A	�A�yAdZA��AX@��;@��@��@�M�@�?}@���@�
=@�%@�1@��m@�+@�\@�Ĝ@��@��H@���@���@�Ĝ@���@�x�@���@��m@�dZ@�ȴ@�!@�n�@��@���@�7L@�A�@�Ĝ@��@�v�@�-@�{@��;@�R@���@���@��@�@�@߶F@�\)@�|�@�~�@��T@���@�E�@�ff@�-@ܼj@��@ف@׮@�+@�9X@���@�&�@��/@�I�@��m@��
@��;@׾w@�\)@�@���@�ff@���@��`@�1@љ�@��m@�ƨ@�z�@���@���@�"�@�ȴ@·+@�=q@��#@�Ĝ@�dZ@ʰ!@�ff@ɺ^@�?}@��`@ȣ�@�  @�o@�=q@�%@�b@��@őh@�j@�;d@�`B@�@��T@�Z@��
@�\)@���@�^5@�$�@��T@�7L@��@���@�z�@�j@�bN@�(�@��@��@���@���@�l�@��+@�/@�I�@�"�@�7L@�Q�@��9@�1'@��F@��y@�@��^@�O�@���@�j@�(�@��m@�A�@�r�@� �@�ƨ@��F@���@��B@~�@q��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A���A���A�ƨA���A���A�$�A�E�A�bNA�r�A�|�AڅAړuAڬAڶFAڥ�Aڟ�Aڛ�A�v�A�1'A��A�  A؝�A��HAדuAו�A�5?A֝�A�p�A�ĜA�O�Aԕ�A��AсA� �A���A�\)A��Aω7A�7LA��;A�(�A���A�Aͣ�A͑hA�r�A� �A��/A��A���A�-A�bNA�1'A��#A��A��A���A�ffA��A�XA�A��A��wA��\A���A�l�A��FA�bNA�I�A�(�A�VA��^A�A���A�-A��hA��A��PA�|�A��\A�;dA�ȴA��7A��wA�bA�A�
=A�n�A�9XA��RA��FA�ƨA�x�A�XA�=qA��A��jA�/A��PA��A�%A�XA��TA��-A�/A�K�A�x�A�n�A�1A}C�A{�Av�Arv�Am�Ai�PAf�uAcp�A`�yA^��A]�PA\ȴA[AY�#AX��AVJASC�APz�AOx�AN9XALbNAJ��AH�AF �AB�!A>�!A<��A:�/A:z�A8~�A6�A6�A5��A3�A/�FA-��A,I�A*��A)�
A&�yA$�HA#�wA"��A" �A ��A $�A/A��AhsA�jAI�A�mAA��A�mA��Ap�A��Ax�A%AA�A1AffAp�A=qA��A%A�DAA�At�AVA;dA�A
�A
��A
�`A
�yA
�jA	�;A	+At�A5?A�9A
�uA	�A	�A�yAdZA��AX@��;@��@��@�M�@�?}@���@�
=@�%@�1@��m@�+@�\@�Ĝ@��@��H@���@���@�Ĝ@���@�x�@���@��m@�dZ@�ȴ@�!@�n�@��@���@�7L@�A�@�Ĝ@��@�v�@�-@�{@��;@�R@���@���@��@�@�@߶F@�\)@�|�@�~�@��T@���@�E�@�ff@�-@ܼj@��@ف@׮@�+@�9X@���@�&�@��/@�I�@��m@��
@��;@׾w@�\)@�@���@�ff@���@��`@�1@љ�@��m@�ƨ@�z�@���@���@�"�@�ȴ@·+@�=q@��#@�Ĝ@�dZ@ʰ!@�ff@ɺ^@�?}@��`@ȣ�@�  @�o@�=q@�%@�b@��@őh@�j@�;d@�`B@�@��T@�Z@��
@�\)@���@�^5@�$�@��T@�7L@��@���@�z�@�j@�bN@�(�@��@��@���@���@�l�@��+@�/@�I�@�"�@�7L@�Q�@��9@�1'@��F@��y@�@��^@�O�@���@�j@�(�@��m@�A�@�r�@� �@�ƨ@��F@���@��B@~�@q��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
R�B
S�B
R�B
R�B
T�B
dZB
�B
��B
��B
�B
�9B
�XB
�}B
ɺB
��B
��B
��B
��B
��B
��B
��B
ȴB
ÖB
��B
�B
�ZB
�ZB
�5B
�TB
�sB
�BJBB
��B
��B
�B
�B
��BBbB(�B"�B#�B1'B9XB>wBG�BN�BK�B�1B�LBƨB�B�BB�B)�BJ�Bs�Bu�Bq�Bk�BiyB`BBm�B�+B��B�B�qB�B�BW
B^5Bm�BcTB|�B�B_;BQ�BZBgmB_;BXBE�B2-B �BVBB  B��B�mB�B��BƨBŢBÖB��B�B��B� Bn�BS�B2-BB
�B
�uB
VB
!�B
	7B	��B	�NB	��B	�RB	��B	y�B	^5B	J�B	7LB	+B	!�B	�B	�B	�B	VB	%B�B�NB�#B�/B�B�B�yB�TB�B��BƨBB�}B�qB�jB�jB�dB�RB�LB�-B�B�B�B�B�B��B��B��B��B��B��B�B�B�B�-B�FB�FB��BÖBĜBŢB��BɺBÖBɺB�B�HB�5B��B��B��B��BƨBƨBŢBĜBŢB��B�}B�FBȴB�B�B�B�
B�B��B��B�B	�B	�B	oB	JB��B�yB�5B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�
B�B�`B�B��B	B	%B	1B	
=B	bB	�B	�B	$�B	&�B	-B	:^B	@�B	I�B	J�B	G�B	H�B	C�B	<jB	;dB	@�B	C�B	@�B	A�B	D�B	B�B	@�B	@�B	D�B	F�B	J�B	G�B	?}B	?}B	<jB	<jB	D�B	N�B	T�B	VB	XB	ZB	[#B	`BB	e`B	k�B	m�B	p�B	r�B	q�B	r�B	s�B	n�B	l�B	q�B	z�B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�B	� B	~�B	|�B	{�B	{�B	x�B	t�B	q�B	r�B	x�B	�B	�1B	�1B	�%B	�B	�B	~�B	� B	� B	�B	�B	�B	�B	�+B	�1B	�=B	�JB	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�RB	�XB	�XB	�qB	��B
�B
#�B
0!2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   B
R�B
S�B
R�B
R�B
T�B
dZB
�B
��B
��B
�B
�9B
�XB
�}B
ɺB
��B
��B
��B
��B
��B
��B
��B
ȴB
ÖB
��B
�B
�ZB
�ZB
�5B
�TB
�sB
�BJBB
��B
��B
�B
�B
��BBbB(�B"�B#�B1'B9XB>wBG�BN�BK�B�1B�LBƨB�B�BB�B)�BJ�Bs�Bu�Bq�Bk�BiyB`BBm�B�+B��B�B�qB�B�BW
B^5Bm�BcTB|�B�B_;BQ�BZBgmB_;BXBE�B2-B �BVBB  B��B�mB�B��BƨBŢBÖB��B�B��B� Bn�BS�B2-BB
�B
�uB
VB
!�B
	7B	��B	�NB	��B	�RB	��B	y�B	^5B	J�B	7LB	+B	!�B	�B	�B	�B	VB	%B�B�NB�#B�/B�B�B�yB�TB�B��BƨBB�}B�qB�jB�jB�dB�RB�LB�-B�B�B�B�B�B��B��B��B��B��B��B�B�B�B�-B�FB�FB��BÖBĜBŢB��BɺBÖBɺB�B�HB�5B��B��B��B��BƨBƨBŢBĜBŢB��B�}B�FBȴB�B�B�B�
B�B��B��B�B	�B	�B	oB	JB��B�yB�5B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�
B�B�`B�B��B	B	%B	1B	
=B	bB	�B	�B	$�B	&�B	-B	:^B	@�B	I�B	J�B	G�B	H�B	C�B	<jB	;dB	@�B	C�B	@�B	A�B	D�B	B�B	@�B	@�B	D�B	F�B	J�B	G�B	?}B	?}B	<jB	<jB	D�B	N�B	T�B	VB	XB	ZB	[#B	`BB	e`B	k�B	m�B	p�B	r�B	q�B	r�B	s�B	n�B	l�B	q�B	z�B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�B	� B	~�B	|�B	{�B	{�B	x�B	t�B	q�B	r�B	x�B	�B	�1B	�1B	�%B	�B	�B	~�B	� B	� B	�B	�B	�B	�B	�+B	�1B	�=B	�JB	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�RB	�XB	�XB	�qB	��B
�B
#�B
0!2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.31 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190518                              AO  ARCAADJP                                                                    20181005190518    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190518  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190518  QCF$                G�O�G�O�G�O�8000            