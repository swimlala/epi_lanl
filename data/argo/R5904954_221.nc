CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  1   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:39Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 4  >4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ?h   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 4  D,   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  E`   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  J$   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 4  N�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  P   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 4  T�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  V   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Z�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 4  _�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  `�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 4  e�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  f�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  k�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    k�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    n�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    q�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  t�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    t�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    t�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    t�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    t�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  t�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    u8   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    uH   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    uL   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         u\   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         u`   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ud   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    uhArgo profile    3.1 1.2 19500101000000  20181005191739  20181005191739  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @���Q��1   @��O��@5:^5?|��d��Q�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @333@�  @�  A��A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B/��B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B���B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B���C   C  C  C  C  C	�fC  C  C  C�C  C  C  C  C  C  C   C"�C$  C%�fC(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CE�fCH  CJ  CL  CM�fCP  CR�CT  CV�CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cy�fC{�fC~  C�fC��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C��C��C�  C�  C�  C�  C��3C�  C��C�  C�  C��C��C��C�  C�  C�  C��C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��C�  C��3C��3C��3C��3C�  C��C��C��C��C�  C��C��C��C�  C��3C�  C�  C�  C��3C�  C��C��3C��C��C�  C��fC�  C��C��C��C�  C�  C��C�  C��C�  C�  C�  C��C�  C��C��3C�  C��3C�  C�  C��C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C��C�  C�  C�  C�  D   D � D  Dy�D��D� D  D� D  D� D  Dy�D��D� D  Dy�D��D� D	  D	y�D
  D
� DfD�fDfD� D  D�fDfD�fD��D� DfD�fD  D� DfDy�DfDy�D  D�fD  D� DfD�fD��Dy��D�9�D�}11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @G
=@��@��A�]A$��AD��Ad��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�B=qB	=qB=qB=qB!=qB)=qB0�B9=qBA=qBI=qBQ=qBY=qBa=qBi=qBq=qBy=qB�k�B���B���B���B���B���B���B���B���B���B���B���B���B�k�B���B���B���BĞ�BȞ�B̞�B�k�BԞ�B؞�B���B���B䞸B螸B잸B�B���B���B�k�C O\CO\CO\CO\CO\C
5�CO\CO\CO\Ch�CO\CO\CO\CO\CO\CO\C O\C"h�C$O\C&5�C(O\C*O\C,O\C.O\C0O\C2O\C4O\C6O\C8O\C:O\C<O\C>O\C@O\CBO\CDO\CF5�CHO\CJO\CLO\CN5�CPO\CRh�CTO\CVh�CXO\CZO\C\O\C^O\C`O\CbO\CdO\CfO\ChO\CjO\ClO\CnO\CpO\CrO\CtO\CvO\CxO\Cz5�C|5�C~O\C��C��C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C��C�'�C�'�C�'�C�4{C�'�C�'�C�'�C�'�C�'�C�'�C��C��C�'�C�'�C�'�C�4{C�4{C�'�C�'�C�'�C�'�C��C�'�C�4{C�'�C�'�C�4{C�4{C�4{C�'�C�'�C�'�C�4{C�'�C��C��C��C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C��C��C�4{C�'�C��C��C��C��C�'�C�4{C�4{C�4{C�4{C�'�C�4{C�4{C�4{C�'�C��C�'�C�'�C�'�C��C�'�C�4{C��C�4{C�4{C�'�C�C�'�C�4{C�4{C�4{C�'�C�'�C�4{C�'�C�4{C�'�C�'�C�'�C�4{C�'�C�4{C��C�'�C��C�'�C�'�C�4{C�'�C�4{C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�4{C�'�C�'�C��C�4{C�'�C�'�C�'�C�'�D �D ��D�D�qDqD��D�D��D�D��D�D�qDqD��D�D�qDqD��D	�D	�qD
�D
��D=D�=D=D��D�D�=D=D�=DqD��D=D�=D�D��D=D�qD=D�qD�D�=D�D��D=D�=DqDy�qD�C�D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�v�A�v�A�x�AƁA�~�AƉ7AƏ\Aƛ�AƟ�AƟ�Aƙ�Aƕ�Aƙ�AƝ�AƝ�Aƣ�Aƣ�AƬAƮAƮAƮAư!AƲ-AƲ-AƲ-Aƴ9Aƴ9AƲ-AƲ-AƲ-Aƴ9AƸRAƸRAƸRAƸRAƸRAƺ^Aƺ^AƼjAƼjAƼjAƼjAƾwA�l�A�oA���A���A�^5A���A�jA�z�A�&�A��A�dZA�A���A��TA��#A�oA��7A�x�A�z�A��A�A�33A���A���A�E�A���A��A�v�A�S�A�(�A���A�x�A��A���A��
A��+A��A���A�bNA��A�ZA�E�A��A�{A�9XA��A���A���A�ȴA�bNA���A�n�A�\)A�M�A�C�A��/A��A���A�|�A�A��A�jA���A���A�p�A��A��A��A���A���A�S�A�S�A�S�A��hA�K�A�A}&�A{�^AzE�AwG�Av^5At�jAq��Ao�^An(�Am�Al  AkdZAjffAi�^AiC�Ah1'Ag�7Af�`Ad��A`�A_�A^��A]�wA\��AY"�AW�AVA�AU��ATVAR��AO��ALn�AK�AK�AJȴAJ�AH~�AF�DAE`BAC�#AB �A@��A?�7A?XA?VA>ȴA<��A;�A:�A9A7�A7\)A6�jA5�7A5+A4�RA4  A3/A1�FA0bNA.�A,A�A*��A)l�A)
=A(��A&�A%/A$ffA#�wA"��A!�#A ��A bAVA�AoA��A��A\)AZA��A�AƨAhsA��A7LA$�AC�AbNA�A�A�/A�A��A��A33A
�DA��A-A�-Ap�A^5At�A�DA��A��A �A �9A {@�@�E�@��@��@�&�@��9@�bN@�9X@��@��-@��
@��@�-@�p�@��/@�F@��`@���@���@�o@�E�@�J@�p�@�@�9X@�w@��@�J@��D@ݺ^@�/@���@�I�@ۅ@��#@�x�@���@�I�@�b@׾w@�l�@ם�@�\)@��H@��y@���@��@ҸR@�bN@·+@�/@�"�@�E�@��#@�7L@�O�@�X@�V@ȣ�@�r�@ǶF@��@��@�Z@�K�@�v�@�@��/@�9X@��@��@��y@���@�w2@|��@oZ�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�v�A�v�A�x�AƁA�~�AƉ7AƏ\Aƛ�AƟ�AƟ�Aƙ�Aƕ�Aƙ�AƝ�AƝ�Aƣ�Aƣ�AƬAƮAƮAƮAư!AƲ-AƲ-AƲ-Aƴ9Aƴ9AƲ-AƲ-AƲ-Aƴ9AƸRAƸRAƸRAƸRAƸRAƺ^Aƺ^AƼjAƼjAƼjAƼjAƾwA�l�A�oA���A���A�^5A���A�jA�z�A�&�A��A�dZA�A���A��TA��#A�oA��7A�x�A�z�A��A�A�33A���A���A�E�A���A��A�v�A�S�A�(�A���A�x�A��A���A��
A��+A��A���A�bNA��A�ZA�E�A��A�{A�9XA��A���A���A�ȴA�bNA���A�n�A�\)A�M�A�C�A��/A��A���A�|�A�A��A�jA���A���A�p�A��A��A��A���A���A�S�A�S�A�S�A��hA�K�A�A}&�A{�^AzE�AwG�Av^5At�jAq��Ao�^An(�Am�Al  AkdZAjffAi�^AiC�Ah1'Ag�7Af�`Ad��A`�A_�A^��A]�wA\��AY"�AW�AVA�AU��ATVAR��AO��ALn�AK�AK�AJȴAJ�AH~�AF�DAE`BAC�#AB �A@��A?�7A?XA?VA>ȴA<��A;�A:�A9A7�A7\)A6�jA5�7A5+A4�RA4  A3/A1�FA0bNA.�A,A�A*��A)l�A)
=A(��A&�A%/A$ffA#�wA"��A!�#A ��A bAVA�AoA��A��A\)AZA��A�AƨAhsA��A7LA$�AC�AbNA�A�A�/A�A��A��A33A
�DA��A-A�-Ap�A^5At�A�DA��A��A �A �9A {@�@�E�@��@��@�&�@��9@�bN@�9X@��@��-@��
@��@�-@�p�@��/@�F@��`@���@���@�o@�E�@�J@�p�@�@�9X@�w@��@�J@��D@ݺ^@�/@���@�I�@ۅ@��#@�x�@���@�I�@�b@׾w@�l�@ם�@�\)@��H@��y@���@��@ҸR@�bN@·+@�/@�"�@�E�@��#@�7L@�O�@�X@�V@ȣ�@�r�@ǶF@��@��@�Z@�K�@�v�@�@��/@�9X@��@��@��y@���@�w2@|��@oZ�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BL�BN�B]/B�\B��B��B��B��B��B�B�?B�3B�'B�B�B��B��B��B��B��B�oB�DB�+B~�Bw�Bt�Bu�Br�Bm�BcTBXBJ�BC�B=qB7LB/B(�B#�B�BhB1B�mB�B��B�DB{�BaHBN�B>wB%�BB%BhB�B"�B-B2-B$�B �B�BhB	7BDBB
��B
�B
�B
�BB
�)B
�
B
��B
�9B
��B
��B
�\B
�=B
� B
o�B
dZB
]/B
O�B
F�B
:^B
&�B
�B
PB
+B
  B	��B	�B	�B	�yB	�TB	�/B	�B	B	��B	��B	��B	�oB	�=B	v�B	gmB	^5B	XB	F�B	<jB	0!B	�B	JB	
=B	1B	+B	%B��B��B�B�fB�BB�B�B�
B��B��BɺBƨBB�wB�dB�XB�FB�?B�9B�-B�B�B��B��B��B��B��B��B��B��B�{B�uB�oB�hB�\B�JB�=B�1B�%B�B�B�B�B�B�B~�B}�B|�Bz�Bw�Bu�Bq�Bo�Bm�Bk�BhsBgmBffBe`BcTBcTB_;B]/B[#BYBXBYBZBZBXBXBW
BW
BT�BT�BT�BT�BT�BT�BT�BS�BS�BVBZB]/B^5B_;B^5B[#BW
BS�BT�BT�BT�BS�BS�BS�BS�BS�BR�BS�BR�BT�BT�BT�BT�BZBZB`BBbNBbNBdZBe`Bl�Bm�Bn�Bo�Bo�Bo�Bm�BgmBaHB_;B^5B`BBbNBcTBdZBjBo�Bp�Bq�Bp�Bo�Bp�Bo�Bp�Bs�Bt�By�B�B�+B�+B�7B�DB�VB	�OB
YB
722222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BL�BN�B]/B�\B��B��B��B��B��B�B�?B�3B�'B�B�B��B��B��B��B��B�oB�DB�+B~�Bw�Bt�Bu�Br�Bm�BcTBXBJ�BC�B=qB7LB/B(�B#�B�BhB1B�mB�B��B�DB{�BaHBN�B>wB%�BB%BhB�B"�B-B2-B$�B �B�BhB	7BDBB
��B
�B
�B
�BB
�)B
�
B
��B
�9B
��B
��B
�\B
�=B
� B
o�B
dZB
]/B
O�B
F�B
:^B
&�B
�B
PB
+B
  B	��B	�B	�B	�yB	�TB	�/B	�B	B	��B	��B	��B	�oB	�=B	v�B	gmB	^5B	XB	F�B	<jB	0!B	�B	JB	
=B	1B	+B	%B��B��B�B�fB�BB�B�B�
B��B��BɺBƨBB�wB�dB�XB�FB�?B�9B�-B�B�B��B��B��B��B��B��B��B��B�{B�uB�oB�hB�\B�JB�=B�1B�%B�B�B�B�B�B�B~�B}�B|�Bz�Bw�Bu�Bq�Bo�Bm�Bk�BhsBgmBffBe`BcTBcTB_;B]/B[#BYBXBYBZBZBXBXBW
BW
BT�BT�BT�BT�BT�BT�BT�BS�BS�BVBZB]/B^5B_;B^5B[#BW
BS�BT�BT�BT�BS�BS�BS�BS�BS�BR�BS�BR�BT�BT�BT�BT�BZBZB`BBbNBbNBdZBe`Bl�Bm�Bn�Bo�Bo�Bo�Bm�BgmBaHB_;B^5B`BBbNBcTBdZBjBo�Bp�Bq�Bp�Bo�Bp�Bo�Bp�Bs�Bt�By�B�B�+B�+B�7B�DB�VB	�OB
YB
722222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.31 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191739                              AO  ARCAADJP                                                                    20181005191739    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191739  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191739  QCF$                G�O�G�O�G�O�8000            