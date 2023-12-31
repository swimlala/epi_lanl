CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-05-21T01:32:12Z creation; 2022-04-26T16:06:58Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.10   Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_on_dac_decoder_version        $Decoded by SIO: Argo SIO SOLOII V2.6   comment_dmqc_operator         bPRIMARY | https://orcid.org/0000-0003-0805-6570 | John Gilson, Scripps Institution of Oceanography        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    8   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  8   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  8�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  `  9   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        9x   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    9�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    9�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  @  9�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    9�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    9�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  @  9�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  @  :   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  @  :T   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    :�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   axis      T      
resolution        >�E�r�_K   
_FillValue        A.�~            :�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    :�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        ?F�k"kmj   
_FillValue        A.�~            :�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   	valid_min         �V�        	valid_max         @V�        axis      Y      
_FillValue        @�i�            :�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    	valid_min         �f�        	valid_max         @f�        axis      X      
_FillValue        @�i�            :�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    :�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    :�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    :�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    ;    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        =    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     0  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 L  Z8   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     0  a�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 L  ~�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     0  �    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     0  �0   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 L  �`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     0  Ǭ   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 L  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     0  �(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     0 	X   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 L &�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     0 -�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 L K   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     0 RP   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` o�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   o�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   u�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   {�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �4   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �<   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �L   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �T   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �    HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �(   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20210521013212  20220426232405  5906402 5906402 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH, SARAH PURKEY, NATHALIE ZILBERMAN, JOHN GILSON    DEAN ROEMMICH, SARAH PURKEY, NATHALIE ZILBERMAN, JOHN GILSON    PRES            TEMP            PSAL            PRES            TEMP            PSAL                  AA  AOAO8138_008904_019                 8138_008904_019                 2C  2C  DD  SOLO_II                         SOLO_II                         8904                            8904                            V2.6; SBE602 14Jan20            V2.6; SBE602 14Jan20            853 853 @�v@�Q@�v@�Q11  @�v@-�@�v@-�@-5�s�h@-5�s�h�dli�B���dli�B��11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  @�\@B�\@�  @�  @�G�@�G�A ��A��A\)A,(�A@��A`��A�  A�Q�A�  A�  A�  A�  A�  A�A�\)B�
B  B(�B (�B((�B/�
B7�
B@  BH  BP  BW�
B_�
Bg�
Bo�Bx  B�{B�{B�{B�{B�  B�  B�  B�{B�  B��B�  B�{B�{B�  B�  B�  B��B��B�  B�{B�  B��B��B�  B�  B�  B��B��B�  B�  B�  B��B��C��C��C  C  C
  C  C  C
=C  C  C
=C��C  C
=C  C   C!��C$  C&
=C(
=C*
=C,  C.  C0
=C2
=C4
=C6  C7��C9�C<  C>
=C@
=CB  CD  CE��CH  CJ  CL  CM��CO��CR  CT
=CU��CW��CZ  C\  C^
=C`  Ca��Cc��Cf
=Ch{Cj  Cl  Cn
=Co��Cr  Ct
=Cv  Cx
=Cz
=C|
=C~�C�C�
=C�C�  C�  C�C�C�  C���C���C���C���C�  C�  C���C�  C�  C���C���C�  C�  C�  C�  C���C�C�C���C���C�C�  C���C�  C�C�  C�  C���C�  C�C�C�
=C�C�  C�  C�  C�C�C�  C�  C�  C���C���C���C���C�  C�  C�  C�  C�C�C�C�  C�  C�  C�  C�  C���C���C�  C�  C���C�C�C�  C�  C���C�  C�  C�  C�  C�  C�  C�  C�C�  C���C�  C�  C�  C�  C�C�C�C�  C�  C���C�  C�  C�C�
=C�C�  C�C�  C���C���C���C�C�  C�  C���C���C���C���C���C�C�C���C�C�
=C�C�  C�  C���C�C�
=C�  C���C���C���D ��D�D� D  D}qD  D� D�qD� D  D� D�qD}qD  D� D�D��D	  D	}qD	��D
� D  D}qD  D��D  D}qD��D}qD�D��D�D� D  D� D�D��D�D��D�D� D�qD� D  D� D�D� D  D}qD�qD��D�D��D�D��D�D� D  D� D��D}qD�qD� D   D }qD!  D!��D"  D"z�D#  D#��D$  D$z�D$�qD%}qD&  D&� D'  D'��D(  D(}qD)  D)��D*  D*}qD*�qD+��D+�qD,� D-  D-��D.�D.� D.�qD/� D0  D0z�D0�qD1� D1�qD2}qD3  D3z�D4  D4��D5  D5� D6  D6� D7�D7}qD8  D8��D9  D9� D9��D:}qD;�D;� D;��D<� D=�D=}qD>�D>�D?�D?� D@�D@��DA  DA��DBDB��DC  DC��DDDD� DE  DE}qDF  DF��DG  DG}qDH  DH��DH�qDI}qDI��DJ� DKDK��DL�DL� DL�qDM� DNDN��DO�DO��DP  DP� DQ  DQ� DR�DR� DS  DS� DT  DT� DU  DU� DV  DV� DV�qDW}qDX  DX��DY�DY� DZ  DZ��D[  D[}qD\  D\� D]�D]��D^  D^� D_  D_� D`�D`� Da  Da� Db�Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg��Dh  Dh}qDi  Di� Dj  Dj� Dk�Dk� Dk�qDl}qDm  Dm}qDm�qDn�Do�Do}qDo�qDp}qDp��Dq� Dr�Dr�Ds�Ds}qDs�qDt� Du�Du� Dv  Dv��Dw�Dw� Dx�Dx� Dx�qDy� Dy�qDz}qDz�qD{� D|  D|}qD|�qD}}qD}�qD~}qD  D��D�  D�@ D�� D��HD��D�AHD�� D�� D�  D�>�D�� D�� D�HD�B�D�� D���D�  D�AHD�~�D��qD�  D�>�D�� D�� D���D�>�D�~�D�� D�HD�>�D�� D��HD��D�B�D�� D��HD��D�AHD�� D�� D��D�AHD�� D�� D�HD�@ D�~�D�� D�HD�@ D�� D���D���D�=qD�~�D�� D���D�>�D�� D�D�HD�@ D�~�D���D���D�>�D�~�D�� D���D�@ D��HD�� D�  D�AHD��HD��HD�  D�>�D�� D��HD�  D�AHD��HD�� D���D�>�D�� D��qD���D�AHD�~�D���D�HD�@ D�� D��HD���D�@ D��HD�� D���D�>�D�� D�� D���D�@ D�� D�� D�  D�@ D�� D��HD�  D�@ D�� D�� D�  D�>�D��HD��HD�  D�>�D�� D��HD�  D�AHD�� D���D�  D�AHD��HD��HD�  D�@ D��HD�� D���D�B�D�� D�� D�HD�AHD��HD��HD�HD�AHD�� D���D�  D�AHD��HD�� D���D�AHD��HD�� D�  D�AHD�� D�� D�HD�@ D�� D���D���D�>�D�� D�� D�  D�AHD��HD�� D�  D�AHD��HD�� D�  D�@ D�� D���D�  D�AHD��HD��HD�HD�@ D�� D�� D���D�@ D�� D�� D�HD�AHD�� D�� D�  D�>�D�� D�� D�  D�AHD�~�D���D�  D�>�D�~�D�� D�HD�>�D�~�D���D���D�>�D�~�D�� D�  D�>�D�~�D�� D�  D�>�D�� D���D���D�@ D�� D���D�  D�AHD��HD��HD���D�@ D��HD���D���D�@ D�� D�� D���D�@ D��HD�� D�  D�@ D��HD���D�  D�AHD��HD�� D���D�@ D��HD���D�  D�AHD��HD�� D���D�@ D��HD�� D�  D�@ D D¾�D���D�@ DÀ D�� D�HD�@ D�~�D�� D�HD�@ D�~�Dž�D���D�@ Dƀ Dƾ�D���D�>�D�~�D�� D�HD�@ D�}qDȾ�D���D�>�D�~�Dɾ�D���D�>�D�~�D�� D�HD�AHDˁHD�D�  D�>�D�~�D̾�D��qD�>�D̀ D��HD�  D�>�D΀ D�� D�HD�@ D�~�D�� D�HD�AHDЀ D��HD�HD�@ Dр D�� D���D�>�D�~�DҾ�D���D�@ Dӂ�D��HD�  D�@ D�~�DԾ�D���D�>�DՀ D�� D�HD�@ D�~�D�� D�  D�AHD׀ D׾�D�  D�@ D؀ D��HD�HD�AHDفHDپ�D���D�@ Dڀ D��HD�HD�@ D�~�D۾�D��qD�@ D܀ Dܾ�D��qD�>�D݁HD�� D���D�>�D�~�D޽qD�  D�B�D߂�D�� D�  D�AHD�� D�� D���D�=qD� D�� D���D�@ D� D�� D���D�>�D� D�� D�HD�AHD�~�D�� D�  D�>�D�~�D�� D���D�@ D�HD�� D���D�@ D�HD��HD��?�?\)?B�\?�  ?��R?�33?�(�?�@�@(�@+�@=p�@L��@\(�@s33@�  @��@��@���@�G�@�=q@�33@�(�@��
@�=q@�@޸R@��@�{@�A   Az�A�Ap�A�AA�HA\)A#33A(��A-p�A1G�A5A:=qA>�RAC33AG
=AK�AP  ATz�AXQ�A]p�AaG�AeAj�HAn�RAs33AxQ�A|(�A���A��HA���A��A��A��
A�ffA���A��HA�A�  A�=qA���A��RG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             ?�  @�\@B�\@�  @�  @�G�@�G�A ��A��A\)A,(�A@��A`��A�  A�Q�A�  A�  A�  A�  A�  A�A�\)B�
B  B(�B (�B((�B/�
B7�
B@  BH  BP  BW�
B_�
Bg�
Bo�Bx  B�{B�{B�{B�{B�  B�  B�  B�{B�  B��B�  B�{B�{B�  B�  B�  B��B��B�  B�{B�  B��B��B�  B�  B�  B��B��B�  B�  B�  B��B��C��C��C  C  C
  C  C  C
=C  C  C
=C��C  C
=C  C   C!��C$  C&
=C(
=C*
=C,  C.  C0
=C2
=C4
=C6  C7��C9�C<  C>
=C@
=CB  CD  CE��CH  CJ  CL  CM��CO��CR  CT
=CU��CW��CZ  C\  C^
=C`  Ca��Cc��Cf
=Ch{Cj  Cl  Cn
=Co��Cr  Ct
=Cv  Cx
=Cz
=C|
=C~�C�C�
=C�C�  C�  C�C�C�  C���C���C���C���C�  C�  C���C�  C�  C���C���C�  C�  C�  C�  C���C�C�C���C���C�C�  C���C�  C�C�  C�  C���C�  C�C�C�
=C�C�  C�  C�  C�C�C�  C�  C�  C���C���C���C���C�  C�  C�  C�  C�C�C�C�  C�  C�  C�  C�  C���C���C�  C�  C���C�C�C�  C�  C���C�  C�  C�  C�  C�  C�  C�  C�C�  C���C�  C�  C�  C�  C�C�C�C�  C�  C���C�  C�  C�C�
=C�C�  C�C�  C���C���C���C�C�  C�  C���C���C���C���C���C�C�C���C�C�
=C�C�  C�  C���C�C�
=C�  C���C���C���D ��D�D� D  D}qD  D� D�qD� D  D� D�qD}qD  D� D�D��D	  D	}qD	��D
� D  D}qD  D��D  D}qD��D}qD�D��D�D� D  D� D�D��D�D��D�D� D�qD� D  D� D�D� D  D}qD�qD��D�D��D�D��D�D� D  D� D��D}qD�qD� D   D }qD!  D!��D"  D"z�D#  D#��D$  D$z�D$�qD%}qD&  D&� D'  D'��D(  D(}qD)  D)��D*  D*}qD*�qD+��D+�qD,� D-  D-��D.�D.� D.�qD/� D0  D0z�D0�qD1� D1�qD2}qD3  D3z�D4  D4��D5  D5� D6  D6� D7�D7}qD8  D8��D9  D9� D9��D:}qD;�D;� D;��D<� D=�D=}qD>�D>�D?�D?� D@�D@��DA  DA��DBDB��DC  DC��DDDD� DE  DE}qDF  DF��DG  DG}qDH  DH��DH�qDI}qDI��DJ� DKDK��DL�DL� DL�qDM� DNDN��DO�DO��DP  DP� DQ  DQ� DR�DR� DS  DS� DT  DT� DU  DU� DV  DV� DV�qDW}qDX  DX��DY�DY� DZ  DZ��D[  D[}qD\  D\� D]�D]��D^  D^� D_  D_� D`�D`� Da  Da� Db�Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg��Dh  Dh}qDi  Di� Dj  Dj� Dk�Dk� Dk�qDl}qDm  Dm}qDm�qDn�Do�Do}qDo�qDp}qDp��Dq� Dr�Dr�Ds�Ds}qDs�qDt� Du�Du� Dv  Dv��Dw�Dw� Dx�Dx� Dx�qDy� Dy�qDz}qDz�qD{� D|  D|}qD|�qD}}qD}�qD~}qD  D��D�  D�@ D�� D��HD��D�AHD�� D�� D�  D�>�D�� D�� D�HD�B�D�� D���D�  D�AHD�~�D��qD�  D�>�D�� D�� D���D�>�D�~�D�� D�HD�>�D�� D��HD��D�B�D�� D��HD��D�AHD�� D�� D��D�AHD�� D�� D�HD�@ D�~�D�� D�HD�@ D�� D���D���D�=qD�~�D�� D���D�>�D�� D�D�HD�@ D�~�D���D���D�>�D�~�D�� D���D�@ D��HD�� D�  D�AHD��HD��HD�  D�>�D�� D��HD�  D�AHD��HD�� D���D�>�D�� D��qD���D�AHD�~�D���D�HD�@ D�� D��HD���D�@ D��HD�� D���D�>�D�� D�� D���D�@ D�� D�� D�  D�@ D�� D��HD�  D�@ D�� D�� D�  D�>�D��HD��HD�  D�>�D�� D��HD�  D�AHD�� D���D�  D�AHD��HD��HD�  D�@ D��HD�� D���D�B�D�� D�� D�HD�AHD��HD��HD�HD�AHD�� D���D�  D�AHD��HD�� D���D�AHD��HD�� D�  D�AHD�� D�� D�HD�@ D�� D���D���D�>�D�� D�� D�  D�AHD��HD�� D�  D�AHD��HD�� D�  D�@ D�� D���D�  D�AHD��HD��HD�HD�@ D�� D�� D���D�@ D�� D�� D�HD�AHD�� D�� D�  D�>�D�� D�� D�  D�AHD�~�D���D�  D�>�D�~�D�� D�HD�>�D�~�D���D���D�>�D�~�D�� D�  D�>�D�~�D�� D�  D�>�D�� D���D���D�@ D�� D���D�  D�AHD��HD��HD���D�@ D��HD���D���D�@ D�� D�� D���D�@ D��HD�� D�  D�@ D��HD���D�  D�AHD��HD�� D���D�@ D��HD���D�  D�AHD��HD�� D���D�@ D��HD�� D�  D�@ D D¾�D���D�@ DÀ D�� D�HD�@ D�~�D�� D�HD�@ D�~�Dž�D���D�@ Dƀ Dƾ�D���D�>�D�~�D�� D�HD�@ D�}qDȾ�D���D�>�D�~�Dɾ�D���D�>�D�~�D�� D�HD�AHDˁHD�D�  D�>�D�~�D̾�D��qD�>�D̀ D��HD�  D�>�D΀ D�� D�HD�@ D�~�D�� D�HD�AHDЀ D��HD�HD�@ Dр D�� D���D�>�D�~�DҾ�D���D�@ Dӂ�D��HD�  D�@ D�~�DԾ�D���D�>�DՀ D�� D�HD�@ D�~�D�� D�  D�AHD׀ D׾�D�  D�@ D؀ D��HD�HD�AHDفHDپ�D���D�@ Dڀ D��HD�HD�@ D�~�D۾�D��qD�@ D܀ Dܾ�D��qD�>�D݁HD�� D���D�>�D�~�D޽qD�  D�B�D߂�D�� D�  D�AHD�� D�� D���D�=qD� D�� D���D�@ D� D�� D���D�>�D� D�� D�HD�AHD�~�D�� D�  D�>�D�~�D�� D���D�@ D�HD�� D���D�@ D�HD��HG�O�?�?\)?B�\?�  ?��R?�33?�(�?�@�@(�@+�@=p�@L��@\(�@s33@�  @��@��@���@�G�@�=q@�33@�(�@��
@�=q@�@޸R@��@�{@�A   Az�A�Ap�A�AA�HA\)A#33A(��A-p�A1G�A5A:=qA>�RAC33AG
=AK�AP  ATz�AXQ�A]p�AaG�AeAj�HAn�RAs33AxQ�A|(�A���A��HA���A��A��A��
A�ffA���A��HA�A�  A�=qA���A��RG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��;A��A���A��
A��
A��/A��/A��A��;A��`A�ĜA�VA�?}A�$�A��A�
=A��A��A��yA��mA��`A��TA��/A��#A��
A���A���A���A���A���A���A���A���A�ƨA�A���A̺^A̩�A�~�A��A˝�A�ĜAɍPA�?}A��`A�x�A���Aǧ�A�Q�A�33A��AƉ7A�G�A�A��/A���A���Aŝ�A�1'A�VA��A��HA���AĴ9Aħ�Aė�Ać+AăA�dZA�;dA��A×�A�S�A�|�A���A��7A�XA��A�hsA��hA�S�A��TA��jA�S�A��9A�
=A�z�A�A�-A�ffA���A��+A�bNA��A�G�A�
=A��A��wA�M�A��yA�v�A���A�E�A��wA�-A�  A��\A�?}A���A�~�A��A�?}A�1A�%A�ffA�ZA��-A�M�A�ƨA�A���A�33A�"�A���A��A��wA��/A�33A��A��\A�l�A��FA��A�O�A��A{�Aqx�AgoAa/A\�/AV��AQAL�AIƨAF�AC�ABI�A@ȴA?��A>  A<�9A;�TA:��A8VA6�RA6ĜA6�A5�;A5oA5A6��A6�A6$�A3x�A21A1+A0I�A/G�A-�;A-|�A-C�A-��A.1'A.bNA.r�A.v�A.�+A.�!A.jA+��A&~�A#
=A!C�A�A��AA�A�wAdZA�A�^A��A%A��AZA�FA��A$�A��AVA��A�9A��A~�AZA(�A��A�#A��A�-A��A�
A{A��A�A��A��A�AĜA�+Av�AQ�A�;AO�A�jA�AM�A�A�7At�AXA7LA�jA��A~�AbNA �A�wA��AI�Az�AZA1A��A�jA �A��A��A+A��A~�A�mA�A`BA/A
��A
I�A	�;A	\)A	/A��A�/A�AM�A��A�
A��A+A�HA��A=qA�A�A��A��Ar�AJA��A��A�AdZAG�A
=A�A�Av�A�AXA �HA �+A E�@���@��H@��@�hs@��/@��@�$�@���@��@�r�@��w@�|�@��@�n�@��#@�G�@��`@��9@�Z@�@��@�-@��@��@��@�  @��H@���@��-@�%@�j@�9X@�t�@��@陚@�&�@��/@��
@�ȴ@�ff@��@噚@�9@��@�D@�A�@��m@�S�@���@�^5@��@ᙚ@�j@�bN@�9X@�(�@���@߶F@�n�@��T@�?}@ܼj@�bN@��@�E�@��@٩�@�`B@؋D@�S�@��@և+@�V@�-@��@ՙ�@�&�@ԓu@��m@�dZ@��@���@���@�I�@υ@·+@ͩ�@�/@��@��`@��`@̴9@��@��
@��H@�$�@��@���@ɺ^@ɲ-@ə�@�%@��
@�dZ@�"�@�
=@Ɵ�@�-@���@���@�`B@�Ĝ@� �@þw@�@��@��9@��@�Z@�(�@�b@���@�ƨ@��@��P@�t�@��@���@���@�(�@��P@�C�@��!@�ff@�E�@�$�@�@��h@��@�V@��`@���@�bN@�Q�@�9X@���@�S�@���@�v�@�E�@���@���@��@�Ĝ@���@�Z@�t�@��H@�ff@�@��7@�x�@���@��j@�bN@��@�  @���@��H@�V@���@�&�@��@�1@�33@��R@�^5@�$�@�x�@�V@��j@�r�@���@�"�@�
=@���@���@���@�v�@��@��7@�7L@���@��@�1'@���@��m@��F@�K�@���@��!@���@�v�@�-@�J@��@���@�%@��j@�j@�  @�ƨ@��F@�S�@�
=@�@���@�M�@�-@�J@��T@�7L@�&�@��@���@��u@�  @�ƨ@�dZ@��@���@��\@�M�@��@��-@�hs@�7L@��/@���@��u@�  @���@�+@��H@���@��!@�v�@�V@�{@���@��h@�G�@���@���@���@�Z@�9X@�1'@� �@�b@��m@��@�t�@�K�@�+@�@��y@��R@�V@�$�@��@�J@��@�@���@��@�`B@��@���@���@�9X@���@�ƨ@�;d@�o@���@��@���@�v�@�=q@�J@��@��-@��7@�x�@�X@�/@���@���@�r�@�I�@� �@���@��P@�S�@�K�@�33@�
=@��H@���@�v�@�$�@��@�{@���@�7L@���@�Q�@��@��m@��
@�ƨ@��w@�|�@�"�@�
=@���@�ff@�$�@��@���@�x�@�X@�V@��`@���@���@�9X@��
@���@�t�@�C�@�"�@��@���@�M�@�@���@�X@�%@��@��@���@�r�@� �@��@~�y@}O�@|z�@{�
@{��@{��@{t�@{S�@{o@z�\@z-@yhs@xĜ@x  @w�w@w|�@vȴ@u�T@uO�@t��@t�@s�
@sS�@r�H@r=q@rJ@q&�@pĜ@p�@pQ�@p �@pb@o�;@oK�@n��@n@m��@m�-@m?}@mV@lZ@k�
@k"�@j�!@i��@i�7@i�@h�9@hr�@g��@g�P@g\)@fȴ@e�@e�h@e`B@e�@d��@c��@b�@a�@ahs@a&�@`�`@`�u@`�@`Q�@_�@]��@]p�@\��@\�j@\z�@\9X@[��@[t�@[dZ@[33@[@Z�@Z�@Z�H@Z��@Z^5@Z=q@Z=q@Z�@Y�#@Y�7@Xr�@X  @W�P@Wl�@Wl�@W\)@W�@V��@V5?@U/@S��@S�@SS�@SS�@S33@So@R~�@Q��@P��@PbN@O�P@N�y@N$�@N{@M��@M�h@Mp�@MV@L��@L�@Lz�@L1@Kt�@Ko@J�@J�H@J��@J^5@I�^@I�@HQ�@Hb@G�@G+@F�@F�+@F5?@F$�@F@E�T@E�-@E�@D�j@DZ@DI�@D(�@C�F@Ct�@Co@B��@B^5@B�@A��@AX@@�`@@��@@r�@@Q�@?�;@?|�@?;d@>�@>ȴ@>v�@>E�@>5?@>$�@>$�@>{@=`B@<�/@;��@;��@;dZ@;"�@;o@;o@;@:�H@:~�@9��@9&�@9�@8��@8�9@8�@81'@7�@7\)@7
=@6�R@6V@5�@5@5�@5?}@4��@4��@4�@4�j@4�@4z�@4(�@3�m@3ƨ@3�F@3��@3C�@3o@2�H@2�!@2n�@2=q@1�@1�7@1x�@1X@1&�@0��@0�u@0A�@/�@/|�@/;d@/�@.��@.�+@.ff@.5?@.@-�@-��@-��@-O�@-?}@-�@,�/@,��@,z�@,j@,Z@,1@+ƨ@+�@+S�@*�@*�!@*n�@*-@)��@)��@)��@)�7@)x�@)7L@(�`@(�u@(Q�@(b@'�P@'l�@';d@&��@&�@&��@&v�@&ff@&E�@&5?@&$�@%�T@%��@%p�@%O�@%?}@%/@$�@$�j@$�D@$Z@$�@#�m@#��@#t�@#dZ@#S�@#S�@#S�A���A���A��A��A��TA��HA��/A��/A��TA��TA��
A���A��A��
A���A���A��
A���A���A��A��
A��
A��
A��
A��
A��A��A��;A��/A��/A��;A��A��/A��#A��A��A��;A��#A��`A��yA��TA��;A��HA��/A��TA��#AͬA�~�A̓A͋DA�G�A�M�A�G�A�M�A�O�A�I�A�K�A�A�A�?}A�?}A�;dA�5?A�7LA� �A�/A�1'A��A� �A� �A�oA��A��A�{G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             A��A��;A��A���A��
A��
A��/A��/A��A��;A��`A�ĜA�VA�?}A�$�A��A�
=A��A��A��yA��mA��`A��TA��/A��#A��
A���A���A���A���A���A���A���A���A�ƨA�A���A̺^A̩�A�~�A��A˝�A�ĜAɍPA�?}A��`A�x�A���Aǧ�A�Q�A�33A��AƉ7A�G�A�A��/A���A���Aŝ�A�1'A�VA��A��HA���AĴ9Aħ�Aė�Ać+AăA�dZA�;dA��A×�A�S�A�|�A���A��7A�XA��A�hsA��hA�S�A��TA��jA�S�A��9A�
=A�z�A�A�-A�ffA���A��+A�bNA��A�G�A�
=A��A��wA�M�A��yA�v�A���A�E�A��wA�-A�  A��\A�?}A���A�~�A��A�?}A�1A�%A�ffA�ZA��-A�M�A�ƨA�A���A�33A�"�A���A��A��wA��/A�33A��A��\A�l�A��FA��A�O�A��A{�Aqx�AgoAa/A\�/AV��AQAL�AIƨAF�AC�ABI�A@ȴA?��A>  A<�9A;�TA:��A8VA6�RA6ĜA6�A5�;A5oA5A6��A6�A6$�A3x�A21A1+A0I�A/G�A-�;A-|�A-C�A-��A.1'A.bNA.r�A.v�A.�+A.�!A.jA+��A&~�A#
=A!C�A�A��AA�A�wAdZA�A�^A��A%A��AZA�FA��A$�A��AVA��A�9A��A~�AZA(�A��A�#A��A�-A��A�
A{A��A�A��A��A�AĜA�+Av�AQ�A�;AO�A�jA�AM�A�A�7At�AXA7LA�jA��A~�AbNA �A�wA��AI�Az�AZA1A��A�jA �A��A��A+A��A~�A�mA�A`BA/A
��A
I�A	�;A	\)A	/A��A�/A�AM�A��A�
A��A+A�HA��A=qA�A�A��A��Ar�AJA��A��A�AdZAG�A
=A�A�Av�A�AXA �HA �+A E�@���@��H@��@�hs@��/@��@�$�@���@��@�r�@��w@�|�@��@�n�@��#@�G�@��`@��9@�Z@�@��@�-@��@��@��@�  @��H@���@��-@�%@�j@�9X@�t�@��@陚@�&�@��/@��
@�ȴ@�ff@��@噚@�9@��@�D@�A�@��m@�S�@���@�^5@��@ᙚ@�j@�bN@�9X@�(�@���@߶F@�n�@��T@�?}@ܼj@�bN@��@�E�@��@٩�@�`B@؋D@�S�@��@և+@�V@�-@��@ՙ�@�&�@ԓu@��m@�dZ@��@���@���@�I�@υ@·+@ͩ�@�/@��@��`@��`@̴9@��@��
@��H@�$�@��@���@ɺ^@ɲ-@ə�@�%@��
@�dZ@�"�@�
=@Ɵ�@�-@���@���@�`B@�Ĝ@� �@þw@�@��@��9@��@�Z@�(�@�b@���@�ƨ@��@��P@�t�@��@���@���@�(�@��P@�C�@��!@�ff@�E�@�$�@�@��h@��@�V@��`@���@�bN@�Q�@�9X@���@�S�@���@�v�@�E�@���@���@��@�Ĝ@���@�Z@�t�@��H@�ff@�@��7@�x�@���@��j@�bN@��@�  @���@��H@�V@���@�&�@��@�1@�33@��R@�^5@�$�@�x�@�V@��j@�r�@���@�"�@�
=@���@���@���@�v�@��@��7@�7L@���@��@�1'@���@��m@��F@�K�@���@��!@���@�v�@�-@�J@��@���@�%@��j@�j@�  @�ƨ@��F@�S�@�
=@�@���@�M�@�-@�J@��T@�7L@�&�@��@���@��u@�  @�ƨ@�dZ@��@���@��\@�M�@��@��-@�hs@�7L@��/@���@��u@�  @���@�+@��H@���@��!@�v�@�V@�{@���@��h@�G�@���@���@���@�Z@�9X@�1'@� �@�b@��m@��@�t�@�K�@�+@�@��y@��R@�V@�$�@��@�J@��@�@���@��@�`B@��@���@���@�9X@���@�ƨ@�;d@�o@���@��@���@�v�@�=q@�J@��@��-@��7@�x�@�X@�/@���@���@�r�@�I�@� �@���@��P@�S�@�K�@�33@�
=@��H@���@�v�@�$�@��@�{@���@�7L@���@�Q�@��@��m@��
@�ƨ@��w@�|�@�"�@�
=@���@�ff@�$�@��@���@�x�@�X@�V@��`@���@���@�9X@��
@���@�t�@�C�@�"�@��@���@�M�@�@���@�X@�%@��@��@���@�r�@� �@��@~�y@}O�@|z�@{�
@{��@{��@{t�@{S�@{o@z�\@z-@yhs@xĜ@x  @w�w@w|�@vȴ@u�T@uO�@t��@t�@s�
@sS�@r�H@r=q@rJ@q&�@pĜ@p�@pQ�@p �@pb@o�;@oK�@n��@n@m��@m�-@m?}@mV@lZ@k�
@k"�@j�!@i��@i�7@i�@h�9@hr�@g��@g�P@g\)@fȴ@e�@e�h@e`B@e�@d��@c��@b�@a�@ahs@a&�@`�`@`�u@`�@`Q�@_�@]��@]p�@\��@\�j@\z�@\9X@[��@[t�@[dZ@[33@[@Z�@Z�@Z�H@Z��@Z^5@Z=q@Z=q@Z�@Y�#@Y�7@Xr�@X  @W�P@Wl�@Wl�@W\)@W�@V��@V5?@U/@S��@S�@SS�@SS�@S33@So@R~�@Q��@P��@PbN@O�P@N�y@N$�@N{@M��@M�h@Mp�@MV@L��@L�@Lz�@L1@Kt�@Ko@J�@J�H@J��@J^5@I�^@I�@HQ�@Hb@G�@G+@F�@F�+@F5?@F$�@F@E�T@E�-@E�@D�j@DZ@DI�@D(�@C�F@Ct�@Co@B��@B^5@B�@A��@AX@@�`@@��@@r�@@Q�@?�;@?|�@?;d@>�@>ȴ@>v�@>E�@>5?@>$�@>$�@>{@=`B@<�/@;��@;��@;dZ@;"�@;o@;o@;@:�H@:~�@9��@9&�@9�@8��@8�9@8�@81'@7�@7\)@7
=@6�R@6V@5�@5@5�@5?}@4��@4��@4�@4�j@4�@4z�@4(�@3�m@3ƨ@3�F@3��@3C�@3o@2�H@2�!@2n�@2=q@1�@1�7@1x�@1X@1&�@0��@0�u@0A�@/�@/|�@/;d@/�@.��@.�+@.ff@.5?@.@-�@-��@-��@-O�@-?}@-�@,�/@,��@,z�@,j@,Z@,1@+ƨ@+�@+S�@*�@*�!@*n�@*-@)��@)��@)��@)�7@)x�@)7L@(�`@(�u@(Q�@(b@'�P@'l�@';d@&��@&�@&��@&v�@&ff@&E�@&5?@&$�@%�T@%��@%p�@%O�@%?}@%/@$�@$�j@$�D@$Z@$�@#�m@#��@#t�@#dZ@#S�@#S�G�O�A���A���A��A��A��TA��HA��/A��/A��TA��TA��
A���A��A��
A���A���A��
A���A���A��A��
A��
A��
A��
A��
A��A��A��;A��/A��/A��;A��A��/A��#A��A��A��;A��#A��`A��yA��TA��;A��HA��/A��TA��#AͬA�~�A̓A͋DA�G�A�M�A�G�A�M�A�O�A�I�A�K�A�A�A�?}A�?}A�;dA�5?A�7LA� �A�/A�1'A��A� �A� �A�oA��A��A�{G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
Q�B
QB
Q�B
QB
P�B
QNB
P�B
P�B
P�B
PHB
P}B
Q�B
O�B
P}B
P�B
PB
P}B
O�B
O�B
O�B
PB
PB
PB
PB
PB
PB
PB
PB
PHB
PB
PB
PB
PB
O�B
O�B
O�B
OvB
OB
N�B
M�B
LdB
GEB
A B
/OB
*eB
'�B
%zB
%B
#nB
!�B
!�B
"�B
$�B
$tB
&�B
(�B
)�B
-�B
IB
_�B
aHB
cTB
d&B
gB
kB
m�B
poB
rGB
r�B
s�B
s�B
v�B
� B
��B
�3B
�NB
�#B
��B"4B7B>�BP}Bd&B�YB��B��BȴB�<B�2B�ZB��B�%B�>B��BB�B
=B
�BB�B�B$BqB�B#�B%�B)�B,B*�B1'B8B<B=<B<6B5tB$�B	�B��B�>B��B�B��B�XBo�BA�B4�B\B
�&B
�B
��B
��B
b�B
C�B
3hB
+�B
"hB
oB	�9B	�1B	_�B	M�B	49B	0UB	.B	33B	5�B	:�B	=�B	@OB	F�B	Q�B	VB	V�B	W�B	T�B	]/B	i�B	o�B	�4B	�AB	�oB	��B	ʌB	��B	�B	��B	�$B	��B	�B	�FB	�6B	�NB	��B
	B
.B
B
�B
$tB
,�B
6FB
&�B	�B	�#B	�vB	��B	�aB	�_B	�B	��B	��B	��B	��B	��B	�	B	��B	�B	�FB	��B	��B	�0B	��B	��B	�0B	��B	�dB	��B	��B	��B	��B	�B	�<B	��B	��B	�B
B
>wB
D�B
I�B
NB
OBB
OB
PB
PHB
OvB
OBB
P�B
S[B
TaB
TaB
U�B
UgB
V9B
UgB
VB
U�B
T�B
VB
S�B
Q�B
L�B
QB
U2B
U2B
S&B
O�B
K�B
J�B
J�B
JXB
I�B
H�B
HB
FtB
E�B
D�B
D3B
C-B
C�B
A�B
A�B
A�B
A B
@�B
@OB
@B
?HB
?HB
>BB
=<B
<6B
<jB
:�B
;dB
9�B
8B
7�B
8�B
7�B
7�B
7�B
6�B
6FB
5?B
49B
4�B
5B
4B
0�B
/�B
-�B
,B
+�B
+6B
*0B
)*B
*0B
*�B
'�B
%FB
&B
%FB
$@B
#�B
$tB
#�B
$B
$tB
#�B
"hB
#B
 �B
"4B
!�B
 �B
 �B
 \B
!B
OB
7B
B
B
�B
�B
�B
�B
:B
�B
VB
(B
�B
�B
B
B
�B
xB
xB
B
DB
xB

�B

�B

	B

�B

�B
B

�B

�B

�B

�B
PB

	B
	7B

rB
JB
�B
DB
B

�B

rB
B

	B

=B
	�B
	�B
	�B
	lB
	B
	B
�B
�B
�B
�B
�B
%B
�B
�B
�B
�B
�B
SB
�B
MB
�B
B
B
�B
�B
MB
�B
B
�B
�B
�B
�B
�B
%B
�B
�B
�B
�B
%B
%B
YB
�B
%B

�B
�B
	7B
�B
�B
�B
�B
fB
�B
�B
�B
_B
�B
	�B

�B
JB

�B
B
xB
xB
B
B
B
~B
JB
~B
�B
�B
PB
B
PB
�B
�B
�B
JB
JB
�B
JB
~B
�B
~B
�B
VB
�B
(B
�B
�B
\B
.B
�B
bB
bB
�B
bB
4B
4B
�B
:B
�B
�B
B
B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
SB
�B
�B
�B
�B
�B
�B
�B
�B
�B
_B
+B
_B
�B
_B
+B
+B
�B
�B
�B
B
B
�B
kB
7B
B
7B
�B
�B
7B
�B
�B
	B
	B
�B
�B
B
�B
�B
B
IB
B
~B
B
B
�B
�B
B
�B
CB
�B
�B
�B
�B
B
�B
�B
�B
�B
OB
B
�B
�B
�B
�B
!-B
 �B
!bB
!�B
!-B
!�B
!�B
!�B
!�B
"hB
"hB
!�B
"hB
#:B
#�B
#:B
"�B
#:B
#�B
$B
#�B
#�B
#�B
$tB
%B
%�B
%FB
%B
%FB
%FB
%FB
%FB
%�B
%�B
%zB
%zB
%�B
&�B
&�B
&LB
&LB
&�B
'�B
'�B
'�B
'�B
'�B
'�B
)*B
)*B
(�B
(�B
)�B
)�B
)�B
)�B
*0B
*eB
*0B
)�B
,B
,B
,=B
,�B
-B
-CB
-CB
,�B
-CB
,�B
-�B
/B
/�B
/B
/�B
0UB
/�B
/�B
0�B
0�B
1'B
0�B
1�B
2-B
2aB
2aB
2�B
2�B
3�B
3�B
4nB
4�B
4�B
5tB
5�B
5tB
5?B
5tB
6FB
6B
6zB
7�B
8�B
8�B
9XB
9$B
8�B
8�B
8�B
9�B
9�B
9�B
:^B
:�B
;�B
;0B
:�B
;�B
<6B
<jB
<�B
=<B
=�B
=�B
>B
>�B
>�B
?HB
?B
?}B
?�B
?�B
?�B
?�B
@�B
@�B
@�B
@�B
@�B
A B
@�B
A�B
AUB
AUB
A�B
A�B
B'B
B[B
B�B
B�B
C�B
C�B
C�B
D3B
E9B
EmB
EmB
EmB
E�B
E�B
FtB
GEB
G�B
GzB
GzB
G�B
GzB
GzB
HKB
H�B
H�B
H�B
IB
H�B
IB
IRB
I�B
I�B
J#B
J�B
JXB
I�B
JXB
J�B
J�B
JXB
JXB
J#B
J�B
JXB
L0B
K�B
LdB
LdB
LdB
LdB
L�B
LdB
M6B
N<B
OBB
OvB
O�B
OBB
OBB
OB
O�B
O�B
P�B
Q�B
RTB
S�B
S�B
S�B
S&B
R�B
S[B
T�B
T�B
T�B
U2B
U�B
U�B
VB
U�B
U�B
UgB
U�B
VmB
V�B
WsB
WsB
W�B
XyB
X�B
YKB
YB
YB
YB
YB
YB
YB
Z�B
ZB
ZB
ZQB
Z�B
Z�B
[#B
[#B
[WB
[WB
[#B
[�B
\)B
\]B
\]B
[�B
\�B
]/B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
^�B
_B
_�B
_�B
_�B
`B
_�B
_�B
_�B
_�B
`BB
aHB
a�B
a�B
a�B
a|B
a�B
a�B
bB
c B
c B
c B
c�B
c�B
c�B
c�B
c�B
d&B
d�B
d�B
d�B
d&B
d�B
d�B
d�B
e`B
e`B
e`B
e�B
e�B
e�B
e�B
ffB
f�B
gB
g�B
hsB
iDB
iyB
i�B
jKB
jKB
jB
j�B
j�B
j�B
j�B
kB
kB
kQB
k�B
k�B
k�B
k�B
lWB
l"B
lWB
l�B
l�B
l�B
l�B
l�B
m)B
m]B
m�B
m�B
m�B
m�B
ncB
n�B
n�B
ncB
n�B
n�B
o B
o B
o5B
oiB
pB
pB
p;B
poB
qB
qAB
qB
qvB
qAB
qvB
q�B
q�B
q�B
rGB
rGB
r�B
r�B
r�B
r�B
r�B
r�B
sB
r�B
sB
s�B
tB
tB
s�B
tB
s�B
tB
S&B
S[B
OB
U�B
P�B
Q�B
P�B
P}B
OvB
PHB
T�B
P�B
Q�B
Q�B
QNB
R B
P�B
Q�B
PB
PHB
Q�B
QB
P}B
R B
P�B
PB
RTB
O�B
P�B
Q�B
P}B
Q�B
Q�B
O�B
QB
Q�B
PHB
P�B
P}B
O�B
P}B
R�B
OB
Q�B
N�B
Q�B
T�B
TaB
V�B
E�B
U�B
O�B
P}B
K�B
OvB
Q�B
QNB
R�B
P�B
OB
P�B
QB
N�B
VmB
NpB
O�B
P�B
O�B
P}B
Q�B
OvB
OB
P�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             B
RlB
QlB
Q�B
P�B
P�B
Q!B
P�B
QB
P�B
P�B
Q�B
TZB
PDB
Q!B
QB
P{B
Q B
PB
O�B
O�B
P(B
P+B
P?B
P*B
P4B
P(B
P B
P@B
PZB
PB
PB
P'B
PB
PB
P B
O�B
O�B
O�B
P	B
P�B
P7B
MUB
HhB
1�B
-B
*�B
(�B
'@B
%�B
"�B
"�B
$=B
%�B
%cB
'B
(�B
)�B
.fB
J}B
`B
a�B
c�B
d}B
gVB
kIB
m�B
p�B
r^B
swB
t�B
uTB
x�B
�5B
��B
�@B
ԢB
�.BXB+B<�BG6BZHBvzB��B�)B��B�B�sBچB�B��B��B�\B��B�B
KBBB�B�B�B�B�B"tB&�B'CB,�B.VB/NB7�B<@B?�B?(BBVB9�B+B�B 5B�B�?B��B��B��ByHBK�BF�B!�B
�B
�xB
��B
�B
n(B
J)B
5�B
.�B
2EB
�B	�B	�DB	lzB	^9B	BwB	=�B	6�B	;2B	=�B	?�B	A�B	C�B	K]B	UkB	X]B	ZB	^^B	Y_B	]=B	i�B	r�B	�qB	�PB	��B	�xB	��B	��B	�)B	��B	��B	��B	��B	��B	��B	�B	�[B
iB
 B
B
�B
$&B
-�B
>B
4uB
CB	�B	؂B	�fB	�WB	��B	��B	��B	�KB	�GB	�MB	��B	�^B	��B	�B	�aB	��B	�/B	�B	��B	�}B	��B	��B	�8B	��B	�,B	�!B	�?B	�\B	�QB	�oB	׵B	��B
�B
=?B
DSB
J1B
N�B
O�B
O�B
Q�B
R�B
Q�B
PDB
Q�B
T�B
U�B
T�B
VB
VB
XB
U�B
V�B
VeB
V B
W�B
V�B
TUB
L>B
Q�B
V�B
W?B
VrB
RB
M B
K�B
L�B
K�B
KhB
KB
I�B
GB
F�B
GMB
E�B
ECB
E�B
B�B
B�B
BWB
B�B
BB
A�B
@�B
@�B
A@B
?�B
>�B
>)B
=�B
=B
=�B
;�B
9B
9VB
9�B
8�B
8B
8(B
7\B
7QB
5�B
4�B
6�B
7�B
6�B
3?B
1=B
/2B
-�B
.8B
-bB
+�B
*�B
-"B
-�B
(�B
&�B
'�B
&�B
%B
%B
%�B
%B
%hB
%ZB
$rB
#�B
$�B
"B
#�B
"PB
!�B
"6B
#B
!�B
 �B
�B
�B
�B
XB
�B
�B
B
cB
�B
�B
�B
B
�B
,B
B
,B
�B
;B
B
�B
�B
 B
�B
�B
�B
�B
}B

�B
HB
�B
�B
�B
�B

�B
�B
�B
�B
6B
�B
�B
�B
�B
`B
B

_B

'B

XB

]B

RB

�B

YB

5B
	B
	�B

�B
#B
%B
�B
!B
 B
"B
tB
�B
�B
`B
.B
pB
6B
B
�B
�B
LB
BB
pB
wB
�B
�B
�B
�B
B
$B
�B
XB
�B
 B
�B

�B
�B
	|B
	�B
	B
	B
�B
	"B
�B
�B
	.B
�B
�B

HB
�B
�B
�B
�B
zB
5B
�B
qB
hB
�B
UB
fB
�B
2B
`B
xB
YB
�B
�B
�B
�B
�B
�B
�B
QB
(B
2B
#B
�B
lB
�B
OB
5B
�B
:B
�B
qB
�B
�B
�B
�B
?B
8B
�B
oB
�B
]B
�B
�B
�B
 B
MB
"B
ZB
�B
�B
�B
�B
�B
DB
%B
wB
�B
�B
GB
�B
�B
3B
�B
B
�B
�B
�B
�B
�B

B
�B
�B
 B
�B
[B
�B
�B
�B
PB
�B
B
gB
�B
UB
 B
B
�B
B
�B
BB
iB
�B
 B
�B
�B
�B
�B
�B
�B
B
B
�B
;B
�B
YB
\B
�B
�B
�B
VB
B
xB
jB
B
YB
mB
�B
�B
hB
\B
CB
 =B
!zB
 �B
!�B
!�B
!�B
"&B
"�B
"8B
")B
"�B
"�B
"�B
#PB
#�B
#�B
#pB
#.B
#�B
$B
$SB
$<B
$�B
$bB
%_B
&B
&aB
%�B
&ZB
%�B
%�B
%�B
%�B
&?B
&{B
& B
%�B
&GB
&�B
&�B
&�B
&�B
'�B
(wB
(1B
(eB
(hB
(B
(�B
)�B
)RB
)GB
)kB
*B
*	B
*aB
*�B
*VB
*�B
*�B
+�B
-�B
,�B
,�B
-"B
-BB
-uB
-}B
-`B
.B
-JB
.�B
0B
03B
/�B
0hB
0�B
0-B
0�B
0�B
1B
1�B
2B
2�B
2�B
2�B
2�B
3 B
33B
4sB
4�B
5IB
5�B
5�B
68B
6B
5�B
5�B
6nB
7'B
7 B
7�B
9�B
9�B
9�B
9�B
9DB
9#B
9&B
9LB
9�B
:!B
:pB
:�B
;>B
;�B
;vB
;�B
<dB
<�B
<�B
<�B
=�B
>B
>BB
>�B
>�B
?nB
?�B
?VB
?�B
?�B
?�B
?�B
@pB
A"B
AB
AB
@�B
AB
AWB
A�B
BmB
A�B
A�B
B\B
B_B
B�B
B�B
B�B
CUB
C�B
D B
D�B
D�B
E�B
E�B
E�B
E�B
FgB
F�B
GXB
G�B
G�B
G�B
G�B
H B
G�B
H�B
I�B
H�B
H�B
I4B
IiB
I7B
ImB
I�B
J
B
I�B
JXB
J�B
J_B
JB
J�B
KB
J�B
JcB
J�B
JuB
J�B
KrB
L�B
LBB
L�B
LmB
L�B
L�B
MB
L�B
NOB
OqB
O�B
O�B
O�B
OkB
OwB
O�B
P�B
P�B
Q\B
R�B
SB
TB
TB
S�B
SnB
S#B
S�B
T�B
T�B
UGB
U�B
V8B
VB
V*B
U�B
VB
U�B
V�B
W%B
W�B
W�B
W�B
XgB
X�B
YCB
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
ZTB
Z�B
Z=B
ZSB
Z�B
[B
[/B
[�B
[�B
[�B
[�B
[�B
\BB
\wB
\�B
\�B
\yB
]4B
]�B
^B
]�B
]�B
^B
]�B
]�B
]�B
]�B
^eB
_uB
_�B
`HB
`B
` B
`#B
_�B
_�B
`B
`UB
`�B
bB
a�B
a�B
bB
a�B
bB
bB
b�B
c�B
c�B
c�B
c�B
c�B
c�B
d?B
d@B
daB
d�B
d�B
d�B
diB
d�B
eB
e B
ezB
e�B
e�B
e�B
fBB
fDB
fUB
f�B
gB
gwB
g�B
h�B
i�B
i�B
j,B
j�B
j�B
kB
kB
j�B
j�B
kjB
kMB
k_B
k�B
k�B
k�B
k�B
lB
lsB
lXB
l�B
l�B
mB
mB
mB
l�B
m�B
m�B
m�B
nqB
nTB
nTB
n�B
n�B
n�B
n�B
n�B
n�B
o[B
omB
o�B
o�B
phB
p�B
prB
p�B
q]B
quB
q^B
q�B
q_B
q�B
q�B
q�B
rB
r�B
r�B
r�B
r�B
r�B
sB
r�B
r�B
s]B
s:B
scB
tB
tJB
t7B
tB
t"B
s�G�O�B
S&B
S[B
OB
U�B
P�B
Q�B
P�B
P}B
OvB
PHB
T�B
P�B
Q�B
Q�B
QNB
R B
P�B
Q�B
PB
PHB
Q�B
QB
P}B
R B
P�B
PB
RTB
O�B
P�B
Q�B
P}B
Q�B
Q�B
O�B
QB
Q�B
PHB
P�B
P}B
O�B
P}B
R�B
OB
Q�B
N�B
Q�B
T�B
TaB
V�B
E�B
U�B
O�B
P}B
K�B
OvB
Q�B
QNB
R�B
P�B
OB
P�B
QB
N�B
VmB
NpB
O�B
P�B
O�B
P}B
Q�B
OvB
OB
P�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<uL�<#�
<#�
<#�
<-P�<���<$��<9�</�O<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<'�<4�<U#f<#�
<Oq�<�G$<({5<4�<�[_<��/<D�{<#�
<���<�͎<H�J<#�
<#�
<#�
<�}8<�Y�<�g/<��T<W	<���<o��<a�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<k��<1�q<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202204261606322022042616063220220426160632202204261606322022042616063220220426160632SI  SI  ARFMARFM                                                                                                                                                2021052101321220210521013212IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021053104003420210531040034QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021053104003420210531040034QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2022042213364720220422133647IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2022042616064320220426160643IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V1.1V1.1CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2022042616064320220426160643IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2022042616064320220426160643IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                