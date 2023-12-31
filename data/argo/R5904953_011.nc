CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:06:18Z creation      
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
_FillValue                 �  A4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \h   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^\   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �H   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �h   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �l   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �p   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �t   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �x   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005190618  20181005190618  5904953 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6432                            2B  A   APEX                            7467                            062512                          846 @פ�偌1   @פ܎8�@3���+�c��Q�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   A   @�ff@�  A   A   A@  A`  A���A�  A�33A�33A�33A�  A�  A�  B   B  B  B��B   B(  B0  B8  B@  BH  BPffBXffB`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B���C�fC�fC  C�C
�C�C  C  C  C�fC�fC�C  C  C  C�fC"  C$  C%�fC(�C*33C,  C-�fC0  C2  C4  C6  C8  C:�C<  C>  C@�CB�CD�CF�CG�fCJ�CK�fCM�fCP  CR33CT  CU�fCW�fCZ  C\  C^  C`  Cb�Cc�fCf  Ch�Cj�Cl  Cn  Cp�Cr  Ct  Cv  Cx33Cy�fC|�C}�fC��C��3C�  C��3C��C��C�  C��3C��C��C��C��C��C��C�  C�  C�  C��C��C�  C��C�  C��3C��C��C��C��C�  C��3C��3C�  C��C��C��C�  C��C��C��3C��3C�  C��3C�  C��C�  C��3C�  C�  C�  C�  C��fC��3C��C��3C�  C�  C��fC��3C�  C��C��C��C�  C��3C��C�  C��fC�  C�  C��3C��C�  C��3C��3C��3C��3C��3C��3C��3C��3C�  C�  C�  C��C��C�  C�  C�  C�  C��3C�  C��3C��fC��3C��3C��3C�  C��3C��3C�  C��3C�  C��C��C��C�  C��3C��3C�  C��C�  C��C��C��C�  C��3C�  C��C��C��C�  C��3C��C��C�  C��fC��3C�  C�  C��3D �fDfD�fDfDy�D��Dy�D��D� DfD��D  Dy�D  D�fD  Dy�D	fD	�fD
  D
� DfD� D  D� D  D�fD  D� D  D� D��D� DfD�fD  Dy�D  D�fD  D�fD�D�fD  D� D��D� DfD� D�3Dy�DfD�fDfD�fD  D� DfDy�D  D� D  D� D��D s3D!  D!� D!��D"� D#fD#� D$fD$�fD%  D%� D&  D&� D'  D'� D(  D(y�D)  D)�fD*  D*� D+  D+� D,fD,� D-  D-� D-��D.� D/fD/�fD0  D0� D1  D1� D2  D2�fD3fD3y�D3��D4� D5fD5� D5��D6y�D7  D7y�D8  D8� D8��D9y�D9�3D:y�D;  D;� D;��D<� D=  D=y�D>  D>�fD?fD?� D@  D@�fD@��DAy�DA��DBy�DCfDC� DD  DD� DE  DE�fDF  DF� DG  DGy�DG��DH�fDH��DIy�DJfDJ�fDK  DK� DK��DL� DM  DM� DN  DN� DO  DO� DP  DP�fDP��DQy�DR  DRy�DR��DSy�DS��DTy�DT��DU� DV  DV� DW  DW� DXfDX�fDY  DY� DZfDZ�fDZ��D[y�D\fD\� D]fD]� D]��D^� D_fD_� D`  D`� Da  Da� Da��Dby�Dc  Dc�fDd  Ddy�Dd��De� Df  Df� DgfDg�fDh  Dh� DifDi�fDj  Dj� Dk  Dky�Dl  Dl�fDl��Dmy�Dn  Dn� Dn��Do� DpfDp� DqfDq��DrfDr� Dr��Dsy�Dt  Dt�fDufDu�fDv�Dv�fDv��Dw� DwٚDy�=D�Nf11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�(�@�A�HA"�HAB�HAb�HA�=qA�p�A���A���A���A�p�A�p�A�p�B �RB�RB�RBQ�B �RB(�RB0�RB8�RB@�RBH�RBQ�BY�B`�RBh�RBp�RBx�RB�\)B�\)B�\)B�\)B�\)B�\)B�\)B��\B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�(�B�\)B�\)C {CzCzC.CG�C
G�CG�C.C.C.CzCzCG�C.C.C.C zC".C$.C&zC(G�C*aGC,.C.zC0.C2.C4.C6.C8.C:G�C<.C>.C@G�CBG�CDG�CFG�CHzCJG�CLzCNzCP.CRaGCT.CVzCXzCZ.C\.C^.C`.CbG�CdzCf.ChG�CjG�Cl.Cn.CpG�Cr.Ct.Cv.CxaGCzzC|G�C~zC�#�C�
=C�
C�
=C�#�C�#�C�
C�
=C�#�C�0�C�0�C�0�C�#�C�#�C�
C�
C�
C�#�C�#�C�
C�#�C�
C�
=C�#�C�0�C�#�C�#�C�
C�
=C�
=C�
C�#�C�0�C�#�C�
C�#�C�#�C�
=C�
=C�
C�
=C�
C�0�C�
C�
=C�
C�
C�
C�
C��pC�
=C�#�C�
=C�
C�
C��pC�
=C�
C�#�C�0�C�0�C�
C�
=C�#�C�
C��pC�
C�
C�
=C�#�C�
C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
C�
C�
C�#�C�#�C�
C�
C�
C�
C�
=C�
C�
=C��pC�
=C�
=C�
=C�
C�
=C�
=C�
C�
=C�
C�#�C�#�C�0�C�
C�
=C�
=C�
C�#�C�
C�#�C�0�C�0�C�
C�
=C�
C�#�C�0�C�0�C�
C�
=C�#�C�0�C�
C��pC�
=C�
C�
D D ��D�D��D�D�DD�DD��D�D�RD�D�D�D��D�D�D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��DD��D�D��D�D�D�D��D�D��DRD��D�D��DD��D�D��D��D�D�D��D�D��D�D��D�D�D�D��D�D��D D ~�D!�D!��D"D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(�D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3�D4D4��D5�D5��D6D6�D7�D7�D8�D8��D9D9�D9��D:�D;�D;��D<D<��D=�D=�D>�D>��D?�D?��D@�D@��DADA�DBDB�DC�DC��DD�DD��DE�DE��DF�DF��DG�DG�DHDH��DIDI�DJ�DJ��DK�DK��DLDL��DM�DM��DN�DN��DO�DO��DP�DP��DQDQ�DR�DR�DSDS�DTDT�DUDU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[D[�D\�D\��D]�D]��D^D^��D_�D_��D`�D`��Da�Da��DbDb�Dc�Dc��Dd�Dd�DeDe��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk�Dl�Dl��DmDm�Dn�Dn��DoDo��Dp�Dp��Dq�Dq�RDr�Dr��DsDs�Dt�Dt��Du�Du��DvRDv��DwDw��Dw�Dy��D�T)11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A� �A�"�A�"�A�"�A�"�A�"�A�"�A� �A��A���A���A�5?A�I�A��AȰ!A�z�A�;dA���A��mA�bNA���Aŕ�A�jAģ�AÝ�A�$�A�"�A��RA���A��A��A�&�A���A��A�S�A�?}A���A��A�/A�S�A���A�p�A�"�A��A��A���A�"�A��A�1A���A���A�&�A��TA�Q�A�-A��HA�A��+A�A�A�XA�C�A��A��A�p�A�1A�C�A���A�{A�A�O�A��A�1A�%A�ȴA�1A��A��A���A��A��uA��FA�7LA�S�A��uA�E�A��A��A��A+A|��Ax�jAwt�At�Ap�!Ao�PAn5?Ah�HAd�yAa�A_K�A^�A^�A^bA]�hA\  A[�wA[t�AY�AX�AW&�AU�AQoAM\)AK��AK�AK"�AF�AC�AC�AB�HAB�jABVA@ĜA?�TA?VA>  A;�-A8��A7t�A5��A5
=A4��A41A2=qA/�A/�A.�!A-��A,�/A+33A'��A$��A#��A#p�A#"�A"{AA��A��An�A�A��A�mA%A�yAQ�AdZA�A�/A�A`BA7LA�HA5?A�A\)A�A?}A�AA�Al�A	�;A	7LA	�A��A�yAG�AQ�A1'AK�A ȴ@��@���@�
=@��-@�|�@�I�@�{@��@��@�C�@�l�@�@���@�1@�;d@�-@��@�X@��;@◍@��@��y@�`B@���@ە�@���@�Ĝ@�j@�l�@�
=@���@Լj@�I�@���@�J@�C�@�M�@�p�@��`@��@���@�5?@�`B@ȓu@��@ǝ�@��H@��@�x�@��/@�Z@��@ÍP@���@��@�I�@�S�@�^5@���@�V@�bN@��@��F@��P@��@�l�@�t�@�t�@�l�@�o@���@�5?@�hs@��@��@�bN@�1'@��F@�l�@�o@��y@�5?@��-@�Ĝ@�Z@��@���@�l�@�S�@�;d@�o@���@�$�@�X@��@��/@��`@��/@��/@���@��`@��D@���@���@�\)@�+@���@�M�@��@��#@��-@���@��@���@�bN@� �@�|�@�@�ȴ@���@�=q@��#@��#@��T@�z�@�ƨ@�;d@���@��+@�ff@��@���@���@�G�@��D@�1@��;@��m@��m@���@���@�+@��@�$�@��@���@��-@�7L@�&�@��@��@�%@���@�V@��7@��-@�x�@���@�1'@��m@���@�l�@�C�@��@���@��\@�v�@�=q@���@��@�G�@�/@�V@���@�j@�I�@�9X@�(�@�  @�ƨ@��@���@�|�@�\)@��\@�{@�hs@��@���@�  @��w@��w@�C�@��!@���@���@���@���@���@�n�@�M�@���@�7L@��@��/@��@�(�@�t�@�n�@�-@�{@��@�$�@��@�{@�{@���@���@�@���@��7@��h@���@���@��7@��7@�G�@��@�%@���@�A�@��
@��P@�l�@�"�@��y@�~�@�V@�5?@��@��^@�O�@��@�&�@��@��@�%@���@�1'@�  @��P@�|�@�t�@�l�@�S�@��@�^5@���@�&�@��@��@��D@�I�@� �@��m@���@�|�@�dZ@�C�@��@���@���@��\@��+@�v�@�v�@�n�@�n�@�M�@�{@��T@�@���@��7@�hs@�/@���@��D@�Q�@�A�@�1'@�1@��@��m@���@���@�l�@�33@�"�@�
=@�ȴ@�v�@�$�@��^@�p�@��@��@��D@�Q�@�1@\)@~��@~5?@}@}%F@mw211111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A� �A�"�A�"�A�"�A�"�A�"�A�"�A� �A��A���A���A�5?A�I�A��AȰ!A�z�A�;dA���A��mA�bNA���Aŕ�A�jAģ�AÝ�A�$�A�"�A��RA���A��A��A�&�A���A��A�S�A�?}A���A��A�/A�S�A���A�p�A�"�A��A��A���A�"�A��A�1A���A���A�&�A��TA�Q�A�-A��HA�A��+A�A�A�XA�C�A��A��A�p�A�1A�C�A���A�{A�A�O�A��A�1A�%A�ȴA�1A��A��A���A��A��uA��FA�7LA�S�A��uA�E�A��A��A��A+A|��Ax�jAwt�At�Ap�!Ao�PAn5?Ah�HAd�yAa�A_K�A^�A^�A^bA]�hA\  A[�wA[t�AY�AX�AW&�AU�AQoAM\)AK��AK�AK"�AF�AC�AC�AB�HAB�jABVA@ĜA?�TA?VA>  A;�-A8��A7t�A5��A5
=A4��A41A2=qA/�A/�A.�!A-��A,�/A+33A'��A$��A#��A#p�A#"�A"{AA��A��An�A�A��A�mA%A�yAQ�AdZA�A�/A�A`BA7LA�HA5?A�A\)A�A?}A�AA�Al�A	�;A	7LA	�A��A�yAG�AQ�A1'AK�A ȴ@��@���@�
=@��-@�|�@�I�@�{@��@��@�C�@�l�@�@���@�1@�;d@�-@��@�X@��;@◍@��@��y@�`B@���@ە�@���@�Ĝ@�j@�l�@�
=@���@Լj@�I�@���@�J@�C�@�M�@�p�@��`@��@���@�5?@�`B@ȓu@��@ǝ�@��H@��@�x�@��/@�Z@��@ÍP@���@��@�I�@�S�@�^5@���@�V@�bN@��@��F@��P@��@�l�@�t�@�t�@�l�@�o@���@�5?@�hs@��@��@�bN@�1'@��F@�l�@�o@��y@�5?@��-@�Ĝ@�Z@��@���@�l�@�S�@�;d@�o@���@�$�@�X@��@��/@��`@��/@��/@���@��`@��D@���@���@�\)@�+@���@�M�@��@��#@��-@���@��@���@�bN@� �@�|�@�@�ȴ@���@�=q@��#@��#@��T@�z�@�ƨ@�;d@���@��+@�ff@��@���@���@�G�@��D@�1@��;@��m@��m@���@���@�+@��@�$�@��@���@��-@�7L@�&�@��@��@�%@���@�V@��7@��-@�x�@���@�1'@��m@���@�l�@�C�@��@���@��\@�v�@�=q@���@��@�G�@�/@�V@���@�j@�I�@�9X@�(�@�  @�ƨ@��@���@�|�@�\)@��\@�{@�hs@��@���@�  @��w@��w@�C�@��!@���@���@���@���@���@�n�@�M�@���@�7L@��@��/@��@�(�@�t�@�n�@�-@�{@��@�$�@��@�{@�{@���@���@�@���@��7@��h@���@���@��7@��7@�G�@��@�%@���@�A�@��
@��P@�l�@�"�@��y@�~�@�V@�5?@��@��^@�O�@��@�&�@��@��@�%@���@�1'@�  @��P@�|�@�t�@�l�@�S�@��@�^5@���@�&�@��@��@��D@�I�@� �@��m@���@�|�@�dZ@�C�@��@���@���@��\@��+@�v�@�v�@�n�@�n�@�M�@�{@��T@�@���@��7@�hs@�/@���@��D@�Q�@�A�@�1'@�1@��@��m@���@���@�l�@�33@�"�@�
=@�ȴ@�v�@�$�@��^@�p�@��@��@��D@�Q�@�1@\)@~��@~5?@}@}%F@mw211111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B?}B?}B?}B?}B?}BA�BA�BA�BC�BG�BO�B��B��B��B��B��BɺBɺB�B  BVB�B!�B2-BZBiyB�+B�hB�{B��B��B�B�}B��B�
B�5B�TB�mB�`B�NB�#B�B��B��BȴB�B��B�B�HB�ZB��B��B�9B�hB�%B�Bs�Be`BdZBZBO�BL�B?}B%�BVB  B�B�B�B�wB�qB�wB��B�qB�oBhsBK�BD�B1'BbB
��B
�`B
��B
��B
�uB
�7B
o�B
ZB
O�B
/B
+B	��B	�)B	�wB	ĜB	ǮB	��B	�{B	�B	x�B	q�B	q�B	p�B	m�B	e`B	cTB	`BB	W
B	N�B	D�B	:^B	�B		7B	  B��B��B�TB�B��B��B��B��BǮBÖB�}B�qB�XB�?B�!B�B�B��B��B��B��B��B��B��B��B�hB�B�B~�B~�B|�Bz�Bz�B|�B}�B|�B}�B�+B�7B�=B�DB�PB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�{B�uB�{B�oB�oB�PB�JB�bB�hB�uB��B��B��B�{B�bB�DB�JB�JB�DB�DB�oB�{B�{B�{B�{B��B�{B��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�-B�3B�3B�FB�dB�}BBĜBƨBɺB��B��B��B�B�B�B�5B�TB�ZB�ZB�fB�sB�sB�B�B�B��B��B��B	  B	B	B	B	B	B	B	+B	JB	bB	oB	{B	�B	�B	�B	 �B	!�B	#�B	&�B	'�B	'�B	)�B	,B	1'B	2-B	49B	6FB	7LB	7LB	7LB	8RB	9XB	;dB	=qB	B�B	G�B	I�B	I�B	J�B	K�B	L�B	N�B	S�B	VB	W
B	YB	\)B	cTB	dZB	ffB	gmB	hsB	k�B	l�B	o�B	q�B	u�B	v�B	v�B	w�B	y�B	z�B	{�B	~�B	�B	�B	�B	�B	�B	�B	�7B	�PB	�\B	�bB	�bB	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�-B	�9B	�?B	�FB	�FB	�RB	�XB	�jB	�jB	�jB	�qB	�wB	B	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�)B	�/B	�5B	�;B	�BB	�;B	�BB	�BB	�BB	�BB	�BB	�BB	�BB	�NB	�NB	�NB	�TB	�NB	�NB	�NB	�NB	�HB	�;B	�BB	�BB	�HB	�HB	�HB	�HB	�NB	�TB	�ZB	�`B	�`B	�`B	�fB	�mB	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
%B
%B
+B
1B
	7B
	7B

=B

=B
DB
DB
JB
JB
JB
PB
PB
PB
VB
VB
VB
VB
VB
VB
\B
VB
VB
\B
\B
\B
\B
bB
bB
bB
bB
hB
hB
{B
oB
&�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   B?}B?}B?}B?}B?}BA�BA�BA�BC�BG�BO�B��B��B��B��B��BɺBɺB�B  BVB�B!�B2-BZBiyB�+B�hB�{B��B��B�B�}B��B�
B�5B�TB�mB�`B�NB�#B�B��B��BȴB�B��B�B�HB�ZB��B��B�9B�hB�%B�Bs�Be`BdZBZBO�BL�B?}B%�BVB  B�B�B�B�wB�qB�wB��B�qB�oBhsBK�BD�B1'BbB
��B
�`B
��B
��B
�uB
�7B
o�B
ZB
O�B
/B
+B	��B	�)B	�wB	ĜB	ǮB	��B	�{B	�B	x�B	q�B	q�B	p�B	m�B	e`B	cTB	`BB	W
B	N�B	D�B	:^B	�B		7B	  B��B��B�TB�B��B��B��B��BǮBÖB�}B�qB�XB�?B�!B�B�B��B��B��B��B��B��B��B��B�hB�B�B~�B~�B|�Bz�Bz�B|�B}�B|�B}�B�+B�7B�=B�DB�PB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�{B�uB�{B�oB�oB�PB�JB�bB�hB�uB��B��B��B�{B�bB�DB�JB�JB�DB�DB�oB�{B�{B�{B�{B��B�{B��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�-B�3B�3B�FB�dB�}BBĜBƨBɺB��B��B��B�B�B�B�5B�TB�ZB�ZB�fB�sB�sB�B�B�B��B��B��B	  B	B	B	B	B	B	B	+B	JB	bB	oB	{B	�B	�B	�B	 �B	!�B	#�B	&�B	'�B	'�B	)�B	,B	1'B	2-B	49B	6FB	7LB	7LB	7LB	8RB	9XB	;dB	=qB	B�B	G�B	I�B	I�B	J�B	K�B	L�B	N�B	S�B	VB	W
B	YB	\)B	cTB	dZB	ffB	gmB	hsB	k�B	l�B	o�B	q�B	u�B	v�B	v�B	w�B	y�B	z�B	{�B	~�B	�B	�B	�B	�B	�B	�B	�7B	�PB	�\B	�bB	�bB	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�-B	�9B	�?B	�FB	�FB	�RB	�XB	�jB	�jB	�jB	�qB	�wB	B	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�)B	�/B	�5B	�;B	�BB	�;B	�BB	�BB	�BB	�BB	�BB	�BB	�BB	�NB	�NB	�NB	�TB	�NB	�NB	�NB	�NB	�HB	�;B	�BB	�BB	�HB	�HB	�HB	�HB	�NB	�TB	�ZB	�`B	�`B	�`B	�fB	�mB	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
%B
%B
+B
1B
	7B
	7B

=B

=B
DB
DB
JB
JB
JB
PB
PB
PB
VB
VB
VB
VB
VB
VB
\B
VB
VB
\B
\B
\B
\B
bB
bB
bB
bB
hB
hB
{B
oB
&�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.18 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190618                              AO  ARCAADJP                                                                    20181005190618    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190618  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190618  QCF$                G�O�G�O�G�O�8000            