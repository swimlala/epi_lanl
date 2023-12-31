CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T12:58:29Z creation      
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
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20181121125829  20190408133247  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5286                            2C  D   APEX                            6531                            072314                          846 @�͎#���1   @�͎��ۜ@4����m�b�p��
=1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BPffBX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct�Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D�fDfD� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DI��DJ� DKfDK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy��D��D�0 D�vfD��3D�3D�S3D���D���D�fD�L�D�vfD���D��D�FfD�l�D��fD�fD�)�D�l�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�@\AG�A!G�AAG�AaG�A���A���A���A���A���AУ�A��A��B Q�BQ�BQ�BQ�B Q�B(Q�B0Q�B8Q�B@Q�BHQ�BP�RBXQ�B`Q�BhQ�BpQ�BxQ�B�(�B�(�B�(�B�(�B�\)B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�C {C{C{C{C{C
{C{C{C{C{C{C{C{C{C{C{C {C"{C${C&{C({C*{C,{C.{C0{C2{C4{C6{C8{C:{C<{C>{C@{CB{CD{CF{CH{CJ{CL{CN{CP{CR{CT{CV{CX{CZ{C\{C^{C`{Cb{Cd{Cf{Ch{Cj{Cl{Cn{Cp{Cr{Ct.Cv{Cx{Cz{C|{C~{C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=D D �DD�DD�DD�DD�DD�DD�DD��D�D�D	D	�D
D
�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�D D �D!D!�D"D"�D#D#�D$D$�D%D%�D&D&�D'D'�D(D(�D)D)�D*D*�D+D+�D,D,�D-D-�D.D.�D/D/�D0D0�D1D1�D2D2�D3D3�D4D4�D5D5�D6D6�D7D7�D8D8�D9D9�D:D:�D;D;�D<D<�D=D=�D>D>�D?D?�D@D@�DADA�DBDB�DCDC�DDDD�DEDE�DFDF�DGDG�DHDH�DIDI�DI��DJ�DK�DK�DLDL�DMDM�DNDN�DODO�DPDP�DQDQ�DRDR�DSDS�DTDT�DUDU�DVDV�DWDW�DXDX�DYDY�DZDZ�D[D[�D\D\�D]D]�D^D^�D_D_�D`D`�DaDa�DbDb�DcDc�DdDd�DeDe�DfDf�DgDg�DhDh�DiDi�DjDj�DkDk�DlDl�DmDm�DnDn�DoDo�DpDp�DqDq�DrDr�DsDs�DtDt~�Dy��D�\D�2�D�x�D���D��D�U�D��)D��\D��D�O\D�x�D��\D�)D�H�D�o\D���D��D�,)D�o\D��\11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�{A�bA�JA�A��`A��mAв-AЗ�A�?}A�"�A��A�{A��A�
=A���A��`A���A���Aϴ9A�bNA��AάA�I�A�{A��A��A���A��;A�ȴAͶFA͇+A�jA�A�=qA��;A�ffA���A�  A�/A�l�A��A�p�A�ZA��hA�C�A��;A���A��TA� �A��A�S�A�7LA�-A�(�A� �A��A���A��uA�z�A�33A�%A�n�A��A�I�A�O�A��A�1'A�|�A�=qA���A�ĜA��uA�v�A�l�A�ffA��TA�-A��7A���A��!A��A���A�/A�-A� �A�O�A��A��/A���A�1'A�;dA��FA��!A��hA�{A�O�A�|�A�A��9A���A���A��A�oA��A�E�A��HA�XA��A��FA�9XA�oA��
A�Q�A};dAzM�Ax �AtJAm��Ai��Ag�Act�Ab��Ab �A`�!A\�\AZ~�AX��AW�7AV��AU|�AR �AP  ANZAL��AJ$�AI�^AI;dAH�uAG�;AF��ADjAAx�A?�mA>��A=��A<-A:jA7��A6^5A4$�A3+A2Q�A0�A/|�A.r�A-hsA,�DA+�A)��A'�^A&��A&r�A%�-A$��A#�^A"E�A!��A!;dA �uA I�A��A��AXA?}A�^A1A^5A��A$�A`BA��Ax�A/A��AbNAG�A�;A�!A�wA�!AQ�A7LAr�AM�A
M�A
 �A	�A&�Ar�A�A%A��A��A��A33A��A �u@�;d@�v�@��!@�t�@��@�\)@���@��
@��@�@�bN@ꗍ@陚@�F@��@�@�|�@���@��@��/@�I�@�n�@���@��;@ڰ!@�X@���@ו�@֟�@�@�hs@�I�@�Ĝ@�{@�M�@�+@�  @�33@��@�{@�-@��@�v�@�@�-@��T@��@�z�@�X@�A�@Л�@Ѻ^@�+@�K�@ҧ�@�`B@ϥ�@��H@�J@˅@�ȴ@�Q�@�@��/@�S�@+@�n�@�-@�G�@���@��@�(�@� �@�t�@��@��H@�Z@�  @�S�@��!@��@��@�7L@��D@�1@���@��@�
=@�
=@���@��T@� �@�"�@�|�@��@���@�t�@���@�o@�n�@��#@��@��@��9@���@�z�@�j@�I�@��
@��@��@�S�@�33@�
=@���@���@��R@��+@�$�@��@���@�?}@�%@���@��@�Z@��@�K�@��@��H@���@���@�$�@��@��@�j@�1'@��
@��@�|�@�;d@�K�@�l�@��@�dZ@���@�M�@���@��-@�X@��@��9@�r�@���@�;d@��!@�ff@���@�V@�M�@�E�@�-@���@��@��@���@��9@���@�9X@��@��
@���@���@�C�@��@��!@�V@�7L@� �@�b@��w@�@��+@�M�@�`B@�Ĝ@�z�@�7L@��P@�o@��+@�n�@�^5@�J@�x�@��9@�Q�@��@���@��@�ƨ@��w@��
@�;d@�(�@�  @��@��+@�-@�-@�5?@��^@�7L@��j@��
@�33@�"�@�+@�C�@���@���@���@�  @���@�{@��@�V@���@���@�\)@�S�@�
=@�~�@�{@��-@��h@�&�@�`B@�X@�7L@�&�@��@�;d@�
=@���@�E�@���@��@��@�=q@�@�V@�`B@��7@�p�@�`B@�/@��@���@���@��u@�z�@�bN@�A�@�b@���@�dZ@�\)@�\)@�33@�+@�
=@��H@��R@���@�V@���@��^@���@�x�@�?}@�"�@�$�@z�!@r^5@k�@bM�@Y%@P��@JJ@A��@97L@3o@-��@(��@$j@�y@��@5?@33@�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�{A�bA�JA�A��`A��mAв-AЗ�A�?}A�"�A��A�{A��A�
=A���A��`A���A���Aϴ9A�bNA��AάA�I�A�{A��A��A���A��;A�ȴAͶFA͇+A�jA�A�=qA��;A�ffA���A�  A�/A�l�A��A�p�A�ZA��hA�C�A��;A���A��TA� �A��A�S�A�7LA�-A�(�A� �A��A���A��uA�z�A�33A�%A�n�A��A�I�A�O�A��A�1'A�|�A�=qA���A�ĜA��uA�v�A�l�A�ffA��TA�-A��7A���A��!A��A���A�/A�-A� �A�O�A��A��/A���A�1'A�;dA��FA��!A��hA�{A�O�A�|�A�A��9A���A���A��A�oA��A�E�A��HA�XA��A��FA�9XA�oA��
A�Q�A};dAzM�Ax �AtJAm��Ai��Ag�Act�Ab��Ab �A`�!A\�\AZ~�AX��AW�7AV��AU|�AR �AP  ANZAL��AJ$�AI�^AI;dAH�uAG�;AF��ADjAAx�A?�mA>��A=��A<-A:jA7��A6^5A4$�A3+A2Q�A0�A/|�A.r�A-hsA,�DA+�A)��A'�^A&��A&r�A%�-A$��A#�^A"E�A!��A!;dA �uA I�A��A��AXA?}A�^A1A^5A��A$�A`BA��Ax�A/A��AbNAG�A�;A�!A�wA�!AQ�A7LAr�AM�A
M�A
 �A	�A&�Ar�A�A%A��A��A��A33A��A �u@�;d@�v�@��!@�t�@��@�\)@���@��
@��@�@�bN@ꗍ@陚@�F@��@�@�|�@���@��@��/@�I�@�n�@���@��;@ڰ!@�X@���@ו�@֟�@�@�hs@�I�@�Ĝ@�{@�M�@�+@�  @�33@��@�{@�-@��@�v�@�@�-@��T@��@�z�@�X@�A�@Л�@Ѻ^@�+@�K�@ҧ�@�`B@ϥ�@��H@�J@˅@�ȴ@�Q�@�@��/@�S�@+@�n�@�-@�G�@���@��@�(�@� �@�t�@��@��H@�Z@�  @�S�@��!@��@��@�7L@��D@�1@���@��@�
=@�
=@���@��T@� �@�"�@�|�@��@���@�t�@���@�o@�n�@��#@��@��@��9@���@�z�@�j@�I�@��
@��@��@�S�@�33@�
=@���@���@��R@��+@�$�@��@���@�?}@�%@���@��@�Z@��@�K�@��@��H@���@���@�$�@��@��@�j@�1'@��
@��@�|�@�;d@�K�@�l�@��@�dZ@���@�M�@���@��-@�X@��@��9@�r�@���@�;d@��!@�ff@���@�V@�M�@�E�@�-@���@��@��@���@��9@���@�9X@��@��
@���@���@�C�@��@��!@�V@�7L@� �@�b@��w@�@��+@�M�@�`B@�Ĝ@�z�@�7L@��P@�o@��+@�n�@�^5@�J@�x�@��9@�Q�@��@���@��@�ƨ@��w@��
@�;d@�(�@�  @��@��+@�-@�-@�5?@��^@�7L@��j@��
@�33@�"�@�+@�C�@���@���@���@�  @���@�{@��@�V@���@���@�\)@�S�@�
=@�~�@�{@��-@��h@�&�@�`B@�X@�7L@�&�@��@�;d@�
=@���@�E�@���@��@��@�=q@�@�V@�`B@��7@�p�@�`B@�/@��@���@���@��u@�z�@�bN@�A�@�b@���@�dZ@�\)@�\)@�33@�+@�
=@��H@��R@���@�V@���@��^@���@�x�@�?}@�"�@�$�@z�!@r^5@k�@bM�@Y%@P��@JJ@A��@97L@3o@-��@(��@$j@�y@��@5?@33@�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB2-B2-B2-B2-B0!B0!B/B.B,B,B,B-B/B.B/B0!B2-B2-B1'B2-B>wBI�B[#B`BBaHBdZBdZBdZBe`BcTBbNBbNBjBe`Bn�B�7B�Bx�B��B�BB�B
=BB�B.BH�BVB]/B]/B^5B`BB]/B]/B]/B]/BcTBo�B� B�B�B�B�\B�{B�hB�%B{�Bk�BXBz�B��B��B��B�B�B�B�BÖB�B�{B�B��B�Bz�B�%Bn�BW
B+B�`B��BB�B��B�VB}�Bp�B�+B�B�qB�?B�{B�JBP�B
�B
��B
��B
�PB
�\B
r�B
L�B
49B
)�B
�B	�B	��B	�DB	��B	x�B	.B	DB	JB	B	B��B�B�#B��BĜB�^B�-B��B��B�VB�1B~�B|�B{�Bz�By�Bw�Bt�Bq�Bt�Bw�Bw�Bv�Bs�Bn�Bl�Bp�Bo�Bm�Bk�Bl�Bo�Bo�Bl�BiyBe`BcTBcTBcTBaHB_;B`BB]/BYBXBVBT�BS�BQ�BR�BaHB�B�oB��B��B�B�?B�RB�RB�}B��B�wB�LB�RBĜB�
B�B�#B�B��B��B�=Bm�Bl�BiyBdZBdZBhsBl�Bn�Bo�Bp�Bn�BiyB_;BVBO�BJ�BF�BA�B@�B>wB<jB=qBD�BG�BH�BH�BJ�BJ�BH�BF�BE�BD�BH�BI�BO�BQ�BS�BS�BR�BVBXBXBZB\)BbNBm�B{�B�B�\B��B��B�B�B�B�RB�RB�wB��BBBÖBĜBƨB��B��B�;B�HB�;B�5B�)B�)B�B�B�B��B��B��B��B��B�B�B�)B�HB�BB�BB�HB�mB�B��B	
=B	JB	
=B		7B	
=B	PB	VB	bB	uB	uB	oB	{B	�B	�B	�B	�B	�B	�B	�B	!�B	"�B	)�B	2-B	:^B	=qB	?}B	A�B	C�B	E�B	G�B	H�B	K�B	O�B	Q�B	T�B	VB	XB	ZB	]/B	]/B	^5B	_;B	bNB	dZB	e`B	hsB	iyB	k�B	l�B	n�B	q�B	u�B	x�B	x�B	x�B	y�B	{�B	z�B	x�B	x�B	x�B	x�B	z�B	z�B	{�B	|�B	� B	�B	�B	�B	�1B	�PB	�\B	�bB	�hB	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�-B	�3B	�-B	�'B	�!B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�?B	�RB	�FB	�9B	�3B	�3B	�9B	�9B	�-B	�'B	�!B	�!B	�!B	�!B	�'B	�?B	�RB	�RB	�jB	�dB	�XB	�jB	�}B	ȴB	ɺB	ȴB	��B	��B	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�/B	�/B	�;B	�BB	�HB	�NB	�TB	�ZB	�ZB	�`B	�`B	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B
1B
bB
�B
�B
'�B
/B
6FB
<jB
@�B
H�B
O�B
T�B
ZB
^5B
bNB
gmB
k�B
n�B
q�B
s�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B2(B2'B2%B2%B0B0B/B.B, B,B, B-
B/B.B/B0B2'B2'B1!B2&B>pBI�B[B`<Ba@BdSBdSBdUBeYBcPBbFBbEBjyBeZBn�B�.B�Bx�B�|B�=B�B
7BB�B.BH�BU�B])B]&B^-B`<B]*B]'B]%B]*BcMBo�B�B�B�B�B�UB�sB�aB�B{�BkBX	Bz�B��B��B��B��B��B��B�BÏB�B�rB� B��B�Bz�B�Bn�BWB*�B�[B��BB�B��B�OB}�Bp�B�$B�	B�jB�8B�sB�CBP�B
�B
��B
��B
�IB
�WB
r�B
L�B
43B
)�B
~B	�B	��B	�8B	��B	x�B	.
B	:B	CB	B	 �B��B�B�B��BĔB�UB�%B��B��B�NB�)B~�B|�B{�Bz�By�Bw�Bt�Bq�Bt�Bw�Bw�Bv�Bs�Bn�Bl�Bp�Bo�Bm�Bk|Bl�Bo�Bo�Bl�BiqBeWBcLBcLBcKBaAB_1B`9B]$BYBXBU�BT�BS�BQ�BR�Ba>B�B�hB��B��B��B�6B�IB�FB�vB�zB�nB�BB�HBĕB�B�B�B�B��B��B�4Bm�Bl�BiqBdRBdOBhkBl�Bn�Bo�Bp�Bn�BioB_0BU�BO�BJ�BF�BA�B@yB>lB<^B=iBD�BG�BH�BH�BJ�BJ�BH�BF�BE�BD�BH�BI�BO�BQ�BS�BS�BR�BU�BXBXBZB\BbBBm�B{�B��B�QB��B��B�B��B�B�HB�HB�kB�yBBBÍBĒBƞB˽B��B�1B�>B�2B�*B�B�B�B�B�B��B��B��B��B��B�B�B�B�>B�9B�7B�>B�bB�uB��B	
2B	@B	
3B		+B	
3B	EB	LB	UB	iB	iB	fB	sB	�B	�B	�B	�B	}B	�B	�B	!�B	"�B	)�B	2!B	:RB	=hB	?sB	AB	C�B	E�B	G�B	H�B	K�B	O�B	Q�B	T�B	U�B	XB	ZB	]'B	]%B	^(B	_0B	bDB	dQB	eWB	hgB	ioB	k{B	l�B	n�B	q�B	u�B	x�B	x�B	x�B	y�B	{�B	z�B	x�B	x�B	x�B	x�B	z�B	z�B	{�B	|�B	�B	�B	�B	�B	�'B	�EB	�RB	�XB	�_B	�^B	�eB	�kB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	� B	�*B	�#B	�B	�B	�	B	��B	�B	�B	�B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�	B	�B	�B	�B	�B	�5B	�GB	�=B	�-B	�+B	�)B	�/B	�.B	�$B	�B	�B	�B	�B	�B	�B	�3B	�FB	�FB	�_B	�[B	�OB	�]B	�tB	ȫB	ɱB	ȪB	ʷB	ʶB	ɯB	ɲB	ɲB	ʵB	˼B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�&B	�/B	�7B	�=B	�CB	�HB	�RB	�OB	�VB	�TB	�]B	�fB	�oB	�uB	�{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B
$B
WB
{B
�B
'�B
/B
68B
<`B
@{B
H�B
O�B
T�B
ZB
^*B
bDB
gbB
k|B
n�B
q�B
s�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.08 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904081332472019040813324720190408133247  AO  ARCAADJP                                                                    20181121125829    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121125829  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121125829  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190408133247  IP                  G�O�G�O�G�O�                